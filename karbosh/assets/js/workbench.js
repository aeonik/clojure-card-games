(function () {
  "use strict";

  function elementPath(root, element) {
    var parts = [];
    var current = element;

    while (current && current !== root) {
      var parent = current.parentElement;
      var index = 0;
      var sibling = current;

      if (!parent) {
        break;
      }

      while ((sibling = sibling.previousElementSibling)) {
        index += 1;
      }

      parts.push(index);
      current = parent;
    }

    return parts.reverse().join(".");
  }

  function elementByPath(root, path) {
    var current = root;
    var parts = path ? path.split(".") : [];

    for (var i = 0; i < parts.length; i += 1) {
      if (!current || !current.children) {
        return null;
      }

      current = current.children[parseInt(parts[i], 10)];
    }

    return current || null;
  }

  function scrollableElements(root) {
    var elements = [root].concat(Array.prototype.slice.call(root.querySelectorAll("*")));

    return elements.filter(function (element) {
      return element.scrollLeft ||
        element.scrollTop ||
        element.scrollWidth > element.clientWidth ||
        element.scrollHeight > element.clientHeight;
    });
  }

  function captureScrollState(root) {
    var state = {
      windowX: window.scrollX || window.pageXOffset || 0,
      windowY: window.scrollY || window.pageYOffset || 0,
      elements: []
    };

    if (!root) {
      return state;
    }

    scrollableElements(root).forEach(function (element) {
      if (element.scrollLeft || element.scrollTop) {
        state.elements.push({
          path: elementPath(root, element),
          left: element.scrollLeft,
          top: element.scrollTop
        });
      }
    });

    return state;
  }

  function restoreScrollState(root, state) {
    if (!state) {
      return;
    }

    if (root) {
      state.elements.forEach(function (item) {
        var element = elementByPath(root, item.path);

        if (element) {
          element.scrollLeft = item.left;
          element.scrollTop = item.top;
        }
      });
    }

    window.scrollTo(state.windowX, state.windowY);
  }

  function restoreScrollAfterLayout(root, state) {
    restoreScrollState(root, state);

    if (window.requestAnimationFrame) {
      window.requestAnimationFrame(function () {
        restoreScrollState(root, state);
      });
    }

    window.setTimeout(function () {
      restoreScrollState(root, state);
    }, 50);
  }

  function nextMainFromHtml(html) {
    var parser = document.createElement("div");
    parser.innerHTML = html;
    return parser.querySelector("#admin-main");
  }

  function replaceWorkbenchMain(html, previousMain) {
    var current = document.getElementById("admin-main");
    var next = nextMainFromHtml(html);

    if (!current || !next) {
      return false;
    }

    var scrollState = captureScrollState(previousMain || current);
    current.parentNode.replaceChild(next, current);
    restoreScrollAfterLayout(next, scrollState);
    return true;
  }

  function markBusy(form, busy) {
    var submitter = form.__workbenchSubmitter;

    form.classList.toggle("is-busy", busy);

    if (submitter) {
      submitter.disabled = busy;
      submitter.setAttribute("aria-busy", busy ? "true" : "false");
    }
  }

  var submissionQueue = [];
  var submissionActive = false;

  function submitWorkbenchForm(form, submitter) {
    var main = document.getElementById("admin-main");
    var method = (form.getAttribute("method") || "get").toLowerCase();

    if (!main || method !== "post") {
      return false;
    }

    form.__workbenchSubmitter = submitter || null;
    markBusy(form, true);

    submissionQueue.push({
      url: form.getAttribute("action") || window.location.href,
      // The server parses url-encoded bodies only, not multipart FormData.
      // Snapshot the fields now, before any re-render replaces the form.
      body: new URLSearchParams(new FormData(form)),
      form: form
    });
    pumpSubmissionQueue();
    return true;
  }

  function pumpSubmissionQueue() {
    if (submissionActive || submissionQueue.length === 0) {
      return;
    }

    var item = submissionQueue.shift();
    submissionActive = true;

    window.fetch(item.url, {
      method: "POST",
      body: item.body,
      credentials: "same-origin",
      // keepalive lets a save finish even if the page is refreshed mid-flight.
      keepalive: true,
      headers: {
        "X-Karbosh-Workbench": "partial"
      }
    }).then(function (response) {
      if (!response.ok) {
        return response.text().then(function (body) {
          throw new Error(body || ("Workbench action failed with HTTP " + response.status));
        });
      }

      return response.text();
    }).then(function (html) {
      submissionActive = false;

      if (submissionQueue.length > 0) {
        // A newer action is already queued; its response will render the
        // combined result, so skip this stale render.
        pumpSubmissionQueue();
        return;
      }

      if (!replaceWorkbenchMain(html, document.getElementById("admin-main"))) {
        window.location.reload();
      }
    }).catch(function (error) {
      submissionActive = false;
      window.alert(error.message || "Workbench action failed");
      markBusy(item.form, false);
      pumpSubmissionQueue();
    });
  }

  document.addEventListener("click", function (event) {
    var button = event.target && event.target.closest("button[type='submit']");

    if (button && button.form && button.form.closest("#admin-main")) {
      button.form.__workbenchSubmitter = button;
    }
  });

  document.addEventListener("submit", function (event) {
    var form = event.target;

    if (!form || !form.closest("#admin-main")) {
      return;
    }

    if (submitWorkbenchForm(form, form.__workbenchSubmitter || event.submitter)) {
      event.preventDefault();
    }
  });

  document.addEventListener("change", function (event) {
    var form = event.target && event.target.closest(".wb-strategy-card");

    if (form && form.closest("#admin-main")) {
      submitWorkbenchForm(form, null);
    }
  });
}());
