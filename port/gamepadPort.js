function addGamepadPort(elmApp) {


  function copyGamepad(g) {
    return !g ? null : {
      axes: g.axes,
      buttons: g.buttons.map(function (b) { return [ b.pressed, b.value ]; }),
      connected: g.connected,
      id: g.id,
      index: g.index,
    };
  }


  var getGamepads =
    typeof navigator === 'undefined' || typeof navigator.getGamepads !== 'function'
    ? function () { return []; }
    : function () { return [].map.call(navigator.getGamepads(), copyGamepad); }


  var previousTimestamp = performance.now()


  raf();


  function raf() {
    requestAnimationFrame(function (timestamp) {

      elmApp.ports.gamepad.send([
        timestamp - previousTimestamp,
        getGamepads(),
      ]);

      previousTimestamp = timestamp;

      raf();
    });
  }

}
