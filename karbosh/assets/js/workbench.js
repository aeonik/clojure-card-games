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

  function submitWorkbenchForm(form, submitter) {
    var main = document.getElementById("admin-main");
    var method = (form.getAttribute("method") || "get").toLowerCase();
    var action = form.getAttribute("action") || window.location.href;

    if (!main || method !== "post") {
      return false;
    }

    form.__workbenchSubmitter = submitter || null;
    markBusy(form, true);

    window.fetch(action, {
      method: "POST",
      body: new FormData(form),
      credentials: "same-origin",
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
      if (!replaceWorkbenchMain(html, main)) {
        window.location.reload();
      }
    }).catch(function (error) {
      window.alert(error.message || "Workbench action failed");
      markBusy(form, false);
    });

    return true;
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
