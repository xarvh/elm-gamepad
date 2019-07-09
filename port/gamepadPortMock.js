(function mockGetGamepads() {


  // Override the system function, yay JavaScript!
  navigator.getGamepads = function getGamepadsMock() {
    updateMockGamepads();
    return mockGamepads;
  };


  const mockGamepads = [];
  const baseSpeed = 10;
  const keyStateByName = {};
  var activeGamepad = 0;
  var inputSpeedMultiplier = 1;


  function initGamepad(index) {
    return {
      connected: true,
      timestamp: 1,
      axes: [],
      buttons: [],
      id: 'Elm Gamepad mock gamepad',
      index: index,
      mapping: '',
    };
  }


  document.addEventListener('keydown', function onKeyUp(event) {
    const key = event.key;
    const n = +key;

    if (n >= 1 && n <= 4) {
      // Select gamepad
      const index = n - 1;
      mockGamepads[index] = mockGamepads[index] || initGamepad(index);
      activeGamepad = index;
      console.info("Selected gamepad: ", index + 1);

    } else if (n >= 5 && n <= 9) {
      // Select speed
      inputSpeedMultiplier = (n - 4) / 5;
      console.info("Input speed multiplier: ", inputSpeedMultiplier);

    } else if (key === '0') {
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
    const now = Date.now();
    const dt = now - previousTimestamp;
    previousTimestamp = now;

    if (mockGamepads.length < 1) return;
    keyMap.forEach(m => m(mockGamepads[activeGamepad], dt * inputSpeedMultiplier * baseSpeed / 1000));
  }


  const axis = (axisIndex, positiveKeyName, negativeKeyName) => (gamepad, speed) => {
    const isPositive = !!keyStateByName[positiveKeyName];
    const isNegative = !!keyStateByName[negativeKeyName];
    const value = gamepad.axes[axisIndex] || 0;

    const direction = isPositive - isNegative;
    gamepad.axes[axisIndex] =
        direction
          ? clamp(-1, 1, value + direction * speed)
          : recoil(value, speed);
  };


  const analogButton = (buttonIndex, keyNames) => (gamepad, speed) => {
    const isDown = keyNames.some((k) => !!keyStateByName[k]);
    const value = (gamepad.buttons[buttonIndex] || {}).value || 0;

    const direction = +isDown;
    gamepad.buttons[buttonIndex] = {
        pressed: isDown,
        value: direction
          ? Math.min(1, value + direction * speed)
          : recoil(value, speed),
    };
  };


  const digitalButton = (buttonIndex, keyNames) => (gamepad, speed) => {
    const isDown = keyNames.some((k) => !!keyStateByName[k]);
    gamepad.buttons[buttonIndex] = {
        pressed: isDown,
        value: isDown ? 1 : 0,
    };
  };


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


  // Map of keyboard keys to button/axis indices
  const keyMap = [
    // left stick
    axis(0, 'd', 'a'),
    axis(1, 'w', 's'),

    // right stick
    axis(2, 'RightArrow', 'LeftArrow'),
    axis(3, 'UpArrow', 'DownArrow'),

    // face buttons
    digitalButton(0, ['x']),
    digitalButton(1, ['y', 'f']),
    digitalButton(2, [' ']),
    digitalButton(3, ['b', 'e']),

    // bumpers
    digitalButton(4, ['Home']),
    digitalButton(5, ['Enter']),

    // triggers
    analogButton(6, ['Control']),
    analogButton(7, ['Alt']),
  ];
})();
