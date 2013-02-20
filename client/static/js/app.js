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

function ChatController($scope, socket, user) {
    $scope.messages = [];
    $scope.maxLines = 50;

    socket.on('chat', function (data) {
        $scope.addLine(data.username, data.message);
    });

    socket.on('server_message', function (data) {
        $scope.addLine('Server', data.message);
    });

    $scope.addLine = function (username, text) {
        if (text.length) {
            if ($scope.messages.length == $scope.maxLines) {
                $scope.messages.shift();
            }

            $scope.messages.push({username: username, text: text});
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

function LoginController($scope, socket, user) {
    $scope.login = function () {
        user.username = prompt('Choose an username');

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