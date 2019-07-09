function addGamepadPort(elmApp) {
  var localStorageKey = 'elm-gamepad-user-mappings';


  elmApp.ports.saveToLocalStorage && elmApp.ports.saveToLocalStorage.subscribe(function (userMappings) {
    localStorage.setItem(localStorageKey, userMappings);
  });


  var environment = {
    userMappings: localStorage[localStorageKey] || '',
    languages: navigator.languages || [],
  };


  var previousFrame;
  requestAnimationFrame(function () {
    previousFrame = getFrame();
    requestAnimationFrame(onAnimationFrame);
  });


  function onAnimationFrame() {
    requestAnimationFrame(onAnimationFrame);
    updateMockGamepads();

    var currentFrame = getFrame();
    elmApp.ports.onBlob.send([ currentFrame, previousFrame, environment ]);

    previousFrame = currentFrame;
  }


  function getFrame() {
    var rawGamepads = getGamepads();

    var serialisedGamepads = [];
    for (var i = 0; i < rawGamepads.length; i++) {
      var g = rawGamepads[i];

      // All browsers running under Windows 10 will sometimes throw in a zombie gamepad
      // object, unrelated to any physical gamepad and never updated.
      // Since this gamepad has always timestamp == 0, we use timestamp > 0 to discard it.
      if (g && g.connected && g.timestamp > 0) serialisedGamepads.push({
        axes: g.axes,
        buttons: g.buttons.map(function (b) { return [ b.pressed, b.value ]; }),
        id: g.id,
        index: g.index + 1,
        mapping: g.mapping,
      });
    }

    return { gamepads: serialisedGamepads, timestamp: Date.now() };
  }


  // Mock stuff
  var mockGamepads = [];
  var activeGamepad = 0;
  var inputSpeedMultiplier = 1;
  var baseSpeed = 10;
  var keyStateByName = {};


  function getGamepads() { return mockGamepads; }


  function initGamepad(index) {
    var b = () => ({ pressed: false, value: 0 });
    return {
      connected: true,
      timestamp: 1,
      axes: [0, 0, 0, 0],
      buttons: [b(), b(), b(), b()],
      id: 'Elm Gamepad mock gamepad',
      index: index,
      mapping: '',
    };
  }


  document.addEventListener('keydown', function onKeyUp(event) {
    var key = event.key;
    var n = +key;

    if (n >= 1 && n <= 4) {
      // Select gamepad
      var index = n - 1;
      mockGamepads[index] = mockGamepads[index] || initGamepad(index);
      activeGamepad = index;
      console.info("Selected gamepad: ", index + 1);

    } else if (n >= 5 && n <= 9) {
      // Select speed
      inputSpeedMultiplier = (n - 4) / 5;
      console.info("Input speed multiplier: ", inputSpeedMultiplier);

    } else if (n === 0) {
      inputSpeedMultiplier = Infinity;
      console.info("Input speed multiplier: ", inputSpeedMultiplier);

    } else {
      keyStateByName[key] = true;
    }
  });


  document.addEventListener('keyup', function onKeyUp(event) {
      keyStateByName[event.key] = false;
  });


  var previousTimestamp = Date.now();
  function updateMockGamepads() {
    var now = Date.now();
    var dt = now - previousTimestamp;
    previousTimestamp = now;

    if (mockGamepads.length < 1) return;
    keyMap.forEach(m => m(mockGamepads[activeGamepad], dt * inputSpeedMultiplier * baseSpeed / 1000));
  }


  var axis = (axisIndex, positiveKeyName, negativeKeyName) => (gamepad, speed) => {
    var isPositive = !!keyStateByName[positiveKeyName];
    var isNegative = !!keyStateByName[negativeKeyName];

    var direction = isPositive - isNegative;
    return gamepad.axes[axisIndex] =
        direction
          ? clamp(-1, 1, gamepad.axes[axisIndex] + direction * speed)
          : recoil(gamepad.axes[axisIndex], speed);
  }


  var keyMap = [
      // left stick
      axis(0, 'a', 'd'),
      axis(1, 'w', 's'),
  ];


  function clamp(min, max, n) {
    if (n < min) return min;
    if (n > max) return max;
    return n;
  }


  function recoil(v, speed) {
    if (v > 0) return Math.max(0, v - speed);
    if (v < 0) return Math.min(0, v + speed);
    return 0;
  }
}
