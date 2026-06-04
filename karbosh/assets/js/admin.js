(function () {
  "use strict";

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
        window.location.reload();
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
}());
