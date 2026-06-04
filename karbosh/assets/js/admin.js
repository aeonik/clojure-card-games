(function () {
  "use strict";

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
      current.parentNode.replaceChild(next, current);
    } else {
      document.body.insertAdjacentElement("afterbegin", next);
    }
  }

  function applyMainUpdate(html) {
    setMainContent(html);
  }

  function refreshDashboard() {
    return window.fetch(window.location.href, {
      credentials: "same-origin"
    }).then(function (response) {
      if (!response.ok) {
        throw new Error("Refresh failed with HTTP " + response.status);
      }

      return response.text();
    }).then(function (html) {
      setMainContent(html);
    }).catch(function () {
      window.location.reload();
    });
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
        return refreshDashboard();
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
