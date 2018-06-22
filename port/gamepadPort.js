function addGamepadPort(elmApp) {


  var getGamepads =
    typeof navigator.getGamepads === 'function' ? function () { return navigator.getGamepads(); } :
    typeof navigator.webkitGetGamepads === 'function' ? function () { return navigator.webkitGetGamepads(); } :
    function () { return []; }


  var previousBlob;
  requestAnimationFrame(function (timestamp) {
    previousBlob = { gamepads: [], timestamp: timestamp };
    raf();
  })


  function raf() {
    requestAnimationFrame(onAnimationFrame);
  }


  function onAnimationFrame(timestamp) {
    raf();

    var serialisedGamepads = [];
    var gamepads = getGamepads();
    for (var i = 0; i < gamepads.length; i++) {
      serialisedGamepads.push(copyGamepad(gamepads[i]));
    }

    var blob = { gamepads: serialisedGamepads, timestamp: timestamp };

    elmApp.ports.gamepad.send([ blob, previousBlob ]);

    previousBlob = blob;
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
