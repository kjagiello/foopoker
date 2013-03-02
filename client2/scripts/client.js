var uniqueId = function () {
  return '_' + Math.random().toString(36).substr(2, 9);
};

$(function(){
    $('.modal').on('show', function() { console.log('yo'); }).on('shown', function () {
        console.log($(this));
        $(this).find('input:first').focus();
    });

    $('.expandable').click(function () {
        $(this).find('.panel').toggle();
    });

    $('.userbar div[data-toggle="panel"]').click(function () {
        var target = $($(this).data('target'));
        var offsetLeft = $(this).position().left;
        var offsetRight = $(this).parent().outerWidth() - (offsetLeft + $(this).outerWidth());

        $(this).parent().find('div[data-toggle="panel"].expanded').not(this).each(function() {
            var target = $($(this).data('target'));
            target.addClass('hide');
            $(this).removeClass('expanded');
        })

        target.css($(this).css('float'), ($(this).css('float') == 'right' ? offsetRight : offsetLeft) + 'px');

        if ($(this).hasClass('expanded')) {
            target.addClass('hide');
            $(this).removeClass('expanded');
        } else {
            target.removeClass('hide');
            $(this).addClass('expanded');
        }
    });
});

$(function() {
        
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

function ChatController($scope, socket, user) {
    $scope.messages = [];
    $scope.maxLines = 50;

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

            // socket.emit('command', {name: cmd[0], arguments: cmd[1] || ""});
        }
        else {
            // socket.emit('chat', {message: $scope.message});
            $scope.addLine ("krille", $scope.message);
        }
        
        $scope.message = '';
    }
}

function GameController ($scope, user, socket) {
    $scope.user = user;
}


function BrowserController($scope) {
    $scope.$on('$viewContentLoaded', function(){
        $('.scroll-area').jScrollPane({
            horizontalGutter:15,
            verticalGutter:-8,
            'showArrows': false
        });

        $('.jspDrag').hide();
        $('.jspScrollable').mouseenter(function(){
            $(this).find('.jspDrag').stop(true, true).fadeIn('slow');
        });
        $('.jspScrollable').mouseleave(function(){
            $(this).find('.jspDrag').stop(true, true).fadeOut('slow');
        });
    });
}

function RoomController($scope) {
    $scope.$on('$viewContentLoaded', function(){
        $('.seat').click(function () {
            var table = $(this).parent('.poker-table');
            var cards = $(this).find('.card');
            var tablePos = $(table).offset();
            var cardsPos = $(cards).offset();

            var relX = $(table).offset().left - $(cards).offset().left + ($(table).width() / 2) - ($(cards).width() / 2);
            var relY = $(table).offset().top - $(cards).offset().top + ($(table).height() / 2) - ($(cards).width() / 2) - 160;

            console.log(tablePos, cardsPos, relY, relX);

            $(this).toggleClass('folded');

            if ($(this).hasClass('folded')) {
                cards.animate({
                    left: $(cards).position().left + relX, 
                    top: $(cards).position().top + relY
                }, function () {
                    $(cards).fadeOut(function () { $(this).remove(); });
                });
            }
        });
    });
}

app.config(['$routeProvider', function($routeProvider) {
    $routeProvider.when('/', {templateUrl: 'static/templates/login.html', controller: LoginController}).
    when('/browser', {templateUrl: 'static/templates/browser.html', controller: BrowserController}).
    when('/room/:id', {templateUrl: 'static/templates/room.html', controller: RoomController}).
    otherwise({redirectTo: '/'});
}]);