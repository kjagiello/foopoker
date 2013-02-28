var uniqueId = function () {
  return '_' + Math.random().toString(36).substr(2, 9);
};

$(function(){
    $('.modal').on('show', function() { console.log('yo'); }).on('shown', function () {
        console.log($(this));
        $(this).find('input:first').focus();
    })    
});

var app = angular.module('foopoker', []);

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

function LoginController($scope, $location, socket, user) {
    $scope.loginData = {};
    $scope.registerData = {};
    $scope.loginError = '';

    $scope.login = function () {
        socket.emit('login', {
            username: $scope.loginData.username, 
            password: $scope.loginData.password
        }, function (message) {
            if (message.status == 'OK') {
                $('.modal').modal('hide');
                $scope.$apply(function () {
                    $location.path('/browser');
                });
            }
            else {
                $scope.$apply(function () {
                    $scope.loginError = 'Provided username or password was incorrect.';
                });
            }
        });
    }

    $scope.register = function () {
        socket.emit('register', {
            username: $scope.registerData.username, 
            password: $scope.registerData.password
        }, function (message) {
            if (message.status == 'OK') {
                $('.modal').modal('hide');
                $scope.loginData = $scope.registerData;
                $scope.login();
            }
            else {
                $scope.$apply(function () {
                    $scope.registerError = 'The username is already taken.';
                });
            }
        });
    }
}

function GameController ($scope, user, socket) {
    $scope.user = user;
}


function BrowserController($scope) {

}

app.config(['$routeProvider', function($routeProvider) {
    $routeProvider.when('/', {templateUrl: 'static/templates/login.html', controller: LoginController}).
    when('/browser', {templateUrl: 'static/templates/browser.html', controller: BrowserController}).
    otherwise({redirectTo: '/'});
}]);