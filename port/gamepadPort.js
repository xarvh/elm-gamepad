function addGamepadPort(elmApp) {


  var getGamepads =
    typeof navigator.getGamepads === 'function' ? function () { return navigator.getGamepads(); } :
    typeof navigator.webkitGetGamepads === 'function' ? function () { return navigator.webkitGetGamepads(); } :
    function () { return []; }


  var previousTimestamp = performance.now();


  raf();


  function raf() {
    requestAnimationFrame(onAnimationFrame);
  }


  function onAnimationFrame(timestamp) {
    raf();

    elmApp.ports.gamepad.send([
      timestamp - previousTimestamp,
      [].map.call(getGamepads(), copyGamepad),
    ]);

    previousTimestamp = timestamp;
  }


  function copyGamepad(g) {
    return !g ? null : {
      axes: g.axes,
      buttons: g.buttons.map(function (b) { return [ b.pressed, b.value ]; }),
      connected: g.connected,
      id: g.id,
      index: g.index,
      mapping: g.mapping,
      timestamp: g.timestamp,
    };
  }
}
