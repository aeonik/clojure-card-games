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

  function setMainContent(html) {
    if (!html || typeof html !== "string") {
      return;
    }

    var parser = document.createElement("div");
    parser.innerHTML = html;
    var next = parser.querySelector("main");
    var current = document.getElementById("admin-main");
    if (!next) {
      return;
    }

    if (current && current.parentNode) {
      var scrollState = captureScrollState(current);
      var currentHistory = current.querySelector("#admin-history-panel");
      var nextHistory = next.querySelector("#admin-history-panel");

      if (currentHistory &&
          nextHistory &&
          /Historical archive is available/.test(nextHistory.textContent || "")) {
        nextHistory.parentNode.replaceChild(currentHistory.cloneNode(true), nextHistory);
      }

      current.parentNode.replaceChild(next, current);
      restoreScrollAfterLayout(next, scrollState);
    } else {
      document.body.insertAdjacentElement("afterbegin", next);
    }
  }

  function applyMainUpdate(html) {
    setMainContent(html);
  }

  function roomButtons(roomId) {
    var buttons = document.querySelectorAll("[data-delete-room]");
    var matches = [];

    Array.prototype.forEach.call(buttons, function (button) {
      if (button.getAttribute("data-delete-room") === roomId) {
        matches.push(button);
      }
    });

    return matches;
  }

  function removeDeletedRoom(roomId) {
    roomButtons(roomId).forEach(function (button) {
      var row = button.closest("tr");

      if (row && row.parentNode) {
        row.parentNode.removeChild(row);
      }
    });

    var detail = document.getElementById("admin-room-detail");
    var detailButton = detail && detail.querySelector("[data-delete-room]");

    if (detailButton && detailButton.getAttribute("data-delete-room") === roomId) {
      detail.innerHTML = "<p class=\"empty\">Room deleted. Waiting for live update.</p>";
    }
  }

  function openAdminStream() {
    var protocol = window.location.protocol === "https:" ? "wss://" : "ws://";
    var query = window.location.search ? "&" + window.location.search.substring(1) : "";
    var socket = new WebSocket(protocol + window.location.host + "/karbosh/ws?mode=admin" + query);

    socket.onmessage = function (event) {
      if (event.data && event.data.indexOf("<main") !== -1) {
        applyMainUpdate(event.data);
      }
    };

    socket.onerror = function () {
      socket.close();
    };

    socket.onclose = function () {
      window.setTimeout(openAdminStream, 2000);
    };
  }

  function deleteRoom(button) {
    var roomId = button.getAttribute("data-delete-room");
    var originalText = button.textContent;

    if (!roomId) {
      return;
    }

    if (!window.confirm("Delete room " + roomId + "?")) {
      return;
    }

    button.disabled = true;
    button.textContent = "Deleting";

    window.fetch("/karbosh/admin/rooms/" + encodeURIComponent(roomId), {
      method: "DELETE",
      credentials: "same-origin"
    }).then(function (response) {
      if (response.ok) {
        button.textContent = "Deleted";
        removeDeletedRoom(roomId);
        return null;
      }

      return response.text().then(function (body) {
        throw new Error(body || ("Delete failed with HTTP " + response.status));
      });
    }).catch(function (error) {
      window.alert(error.message || "Delete failed");
      button.disabled = false;
      button.textContent = originalText;
    });
  }

  document.addEventListener("click", function (event) {
    var target = event.target;

    if (target && target.hasAttribute("data-delete-room")) {
      deleteRoom(target);
    }
  });

  openAdminStream();
}());
