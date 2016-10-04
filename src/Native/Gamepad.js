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
                    [];

                for (var i = 0; i < gpInList.length; i++) {

                    var gpIn =
                        gpInList[i];

                    if (gpIn) {
                        gpOutList.push({
                            index: gpIn.index,
                            axes: gpIn.axes || [],
                            buttons: (gpIn.buttons || []).map(function (b) { return [ b.pressed, b.value ]; }),
                            connected: !!gpIn.connected,
                        });
                    }
                }

                callback(Task.succeed({ time: time, gamepads: gpOutList }));
            });

            return function() {
                cancelAnimationFrame(id);
            };
        }),
    };
}();
