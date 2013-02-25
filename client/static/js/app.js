var app = angular.module('foopoker', []);

var uniqueId = function () {
  return '_' + Math.random().toString(36).substr(2, 9);
};

// http://jsfiddle.net/bYUa3/2/
app.directive('onKeyup', function() {
    return function(scope, elm, attrs) {
        function applyKeyup() {
          scope.$apply(attrs.onKeyup);
        };           
        
        var allowedKeys = scope.$eval(attrs.keys);
        elm.bind('keyup', function(evt) {
            //if no key restriction specified, always fire
            if (!allowedKeys || allowedKeys.length == 0) {
                applyKeyup();
            } else {
                angular.forEach(allowedKeys, function(key) {
                    if (key == evt.which) {
                        applyKeyup();
                    }
                });
            }
        });
    };
});

app.factory('user', function ($rootScope) {
    var userService = {};

    userService.username = '';

    return userService;
});

app.factory('socket', function ($rootScope) {
    var host = "ws://localhost:9001/";
    var socket = new WebSocket(host);
    var callbacks = {};

    socket.onopen = function () {
        console.log(socket.readyState + ': open')
    }
    
    socket.onerror = function (e) {
        console.log(e); 
    }

    socket.onclose = function() {
        console.log(socket.readyState + ': closed')
    }

    socket.onmessage = function(msg) {
        var d = JSON.parse(msg.data);

        console.log(d);

        if ('ref' in d && 'data' in d) {
            var ref = d['ref'];

            if (ref in callbacks) {
                callbacks[ref](d['data']);
                delete callbacks[ref];
            }
            else {
                console.error('Got back a message we were not expecting.', msg);
            }
        }
        else {
            if ('event' in d && 'data' in d && d['event'] in callbacks) {
                var cs = callbacks[d['event']];

                for (var c in cs) {
                    cs[c](d['data']);
                }
            }
        }
    }

    function registerHandler(e, handler) {
        callbacks[e] = callbacks[e] || [];
        callbacks[e].push(handler);
    }

    return {
        on: function (eventName, callback) {
            registerHandler(eventName, function() {
                var args = arguments;
                $rootScope.$apply(function() {
                    callback.apply(socket, args);
                });
            })
        },

        emit: function (eventName, data, callback) {
            var d = {"event": eventName, "data": data};

            if (callback) {
                d['ref'] = uniqueId();
                callbacks[d['ref']] = callback;
                console.log("register: ", d['ref']);
            }

            socket.send(JSON.stringify(d));
        }
    };
});

function GameController($scope, socket, user) {
    $scope.wallet = 0;
    $scope.username = user.username;

    socket.on('update_money', function (data) {
        $scope.wallet = data.money;
    });
}

function ChatController($scope, socket, user) {
    $scope.messages = [];
    $scope.maxLines = 50;

    socket.on('chat', function (data) {
        $scope.addLine(data.username, data.message);
    });

    socket.on('server_message', function (data) {
        $scope.addLine(data.username, data.message);
    });

    $scope.addLine = function (username, text) {
        if (text.length) {
            if ($scope.messages.length == $scope.maxLines) {
                $scope.messages.shift();
            }

            $scope.messages.push({username: username, text: text.split("\n")});
        }
    }

    $scope.sendMessage = function () {
        if ($scope.message.lastIndexOf('/', 0) === 0) {
            var msg = $scope.message;
            var cmd = msg.substring(1).split(' ');

            socket.emit('command', {name: cmd[0], arguments: cmd[1] || ""});
        }
        else {
            socket.emit('chat', {message: $scope.message});
        }
        
        $scope.message = '';
    }
}

function TableController($scope, socket, user) {
    $scope.seats = [];
    $scope.pot = 0;
    $scope.cards = [];
    $scope.playerCards = [];
    $scope.bestHand = "";

    for (var i = 0; i < 8; i++) {
        $scope.seats[i] = {id: i}
    }

    socket.on('update_pot', function (data) {
        $scope.pot = data.pot;
    });

    socket.on('new_card', function (data) {
        $scope.cards.push(data);
    });

    socket.on('new_player_card', function (data) {
        $scope.playerCards.push(data);
    });

    socket.on('best_hand', function (data) {
        $scope.bestHand = data.hand;
    })
}

function SeatController($scope, $attrs, $timeout, socket, user) {
    $scope.seatId = $attrs.seatId;
    $scope.user = null;
    $scope.timerLast = 0;
    $scope.timer = 100;
    $scope.timertimerHandle = null;

    $scope.countdown = function(){
        var v = Math.round(100 / $scope.timerLast);
        var v = v < 100 ? v : 100;

        $scope.timer = $scope.timer + v;
        $('.timer').trigger('change');
    }

    socket.on('user_join', function (data) {
        if (data.seat != $scope.seatId)
            return;

        $scope.user = data.user;
    });

    socket.on('user_leave', function (data) {
        if (data.seat != $scope.seatId)
            return;

        $scope.user = null;
    });

    socket.on('countdown', function (data) {
        if (data.seat != $scope.seatId) {
            $scope.timer = 100;
            return;
        }

        $timeout.cancel($scope.timerHandle);

        $scope.timer = 0;
        $scope.timerLast = data.time;
    });

    $scope.$watch('timer', function (newValue, oldValue) {
        if ($scope.timer < 100) {
            $scope.timerHandle = $timeout($scope.countdown, 1000);

            $('.timer').trigger('change');
        }
    });

    $scope.sitDown = function () {
        $scope.seatId = $attrs.seatId;
        socket.emit('command', {name: 'sit', arguments: $scope.seatId});
    }
}

app.directive('seat', function () {
    return {
        templateUrl: 'seat.html',
        restrict: 'E',
        scope: {
            seatId: '@',
            user: '@',
            timer: '@'
        },
        controller: SeatController
    }
})

function LoginController($scope, socket, user) {
    $scope.login = function () {
        user.username = prompt('Choose an username'); //uniqueId(); //

        socket.emit('login', {username: user.username}, function (message) {
            console.log(message.status);
            if (message.status == 'OK') {
                $('#login-interface').hide();
                $('#game-interface').show();
                $('body').removeClass('login-interface');
            }
            else {
                $scope.login();
            }
        });
    }
}