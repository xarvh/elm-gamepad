var _xarvh$elm_gamepad$Native_Gamepad = function() {


    var Task = _elm_lang$core$Native_Scheduler;


    var hasGamepads =
        typeof navigator !== 'undefined' &&
        typeof navigator.getGamepads === 'function';


    return {
        animationFrameAndGamepads: Task.nativeBinding(function (callback) {
            var id = requestAnimationFrame(function (time) {

                var gpInList =
                    hasGamepads ? navigator.getGamepads() : [];

                var gpOutList =
                    new Array(gpInList.length);

                for (var index = 0; index < gpInList.length; index++) {

                    var gpIn =
                        gpInList[index];

                    gpOutList[index] = {
                        index: index,
                        axes: !gpIn ? [] : gpIn.axes,
                        buttons: !gpIn ? [] : gpIn.buttons.map(function (b) { return [ b.pressed, b.value ]; }),
                        connected: !gpIn ? false : gpIn.connected,
                    };
                }

                callback(Task.succeed({ time: time, gamepads: gpOutList }));
            });

            return function() {
                cancelAnimationFrame(id);
            };
        }),
    };
}();
