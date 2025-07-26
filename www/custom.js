// Add this JavaScript code to handle progress updates
Shiny.addCustomMessageHandler("updateProgress", function(data) {
  var progressBar = document.getElementById(data.id);
  if (progressBar) {
    progressBar.style.width = data.value + "%";
    progressBar.textContent = data.value + "%";
    progressBar.setAttribute("aria-valuenow", data.value);
  }
});

Shiny.addCustomMessageHandler("updateProgressError", function(data) {
  var progressBar = document.getElementById(data.id);
  if (progressBar) {
    progressBar.classList.remove("progress-bar-striped", "progress-bar-animated");
    progressBar.classList.add("bg-danger");
  }
});
