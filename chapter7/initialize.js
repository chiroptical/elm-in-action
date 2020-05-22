var app = Elm.Main.init({
  node: document.getElementById("app"),
  flags: Pasta.version
});

app.ports.setFilters.subscribe(function(options) {
  window.requestAnimationFrame(function() {
      Pasta.apply(document.getElementById("main-canvas"), options);
  });
});

Pasta.addActivityListener(function(activity) {
  app.ports.activityChanges.send(activity);
});
