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


var app = angular.module('foopoker', []);

app.factory('user', function ($rootScope) {
    var userService = {};

    userService.username = '';
    userService.roomId = null;

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
                user.username = $scope.loginData.username;

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

function ChatController($scope, $attrs, socket, user) {
    $scope.messages = [];
    $scope.maxLines = 50;
    $scope.chatType = $attrs.chatType;

    console.log($scope.chatType);

    socket.on('server_message', function (data) {
        if (data.chatType == $scope.chatType)
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
            socket.emit('chat', {message: $scope.message, chatType: $scope.chatType});
        }
        
        $scope.message = '';
    }
}

app.directive('chat', function () {
    return {
        templateUrl: 'static/templates/chat.html',
        restrict: 'E',
        scope: {
            chatType: '@'
        },
        controller: ChatController
    }
})

function GameController ($scope, user, socket) {
    $scope.user = user;
}


function BrowserController($scope, $location, socket) {
    $scope.rooms = [];

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

        $scope.loadRooms();
    });

    $scope.loadRooms = function () {
        socket.emit('rooms', {}, function (message) {
            $scope.$apply(function () {
                $scope.rooms = message.rooms;
            });
        });
    }

    $scope.enter = function (id) {
        console.log('entering room: ', id);

        socket.emit('enter', {'id': id}, function (message) {
            $scope.$apply(function () {
                $location.path('/room/' + id);
            });
        });
    }
}

function SeatController($scope, $attrs, $timeout, socket, user) {
    $scope.seatId = $attrs.seatId;
    $scope.user = null;
    $scope.timerLast = 0;
    $scope.timer = 100;
    $scope.timertimerHandle = null;
    $scope.avatar = 'http://2.bp.blogspot.com/-RatTLFiu6J4/T5l_v59jbVI/AAAAAAAAQ2A/kelVxm_vcLI/s400/blank_avatar_220.png';

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
        templateUrl: 'static/templates/seat.html',
        restrict: 'E',
        scope: {
            seatId: '@',
            user: '@'
        },
        controller: SeatController
    }
})

function RoomController($scope) {
    $scope.$on('$viewContentLoaded', function(){
        /*$('.seat').click(function () {
            var table = $(this).parents('.poker-table');
            var cards = $(this).find('.card');

            var tablePos = $(table).offset();
            var cardsPos = $(cards).offset();

            var relX = $(table).offset().left - $(cards).offset().left + ($(table).width() / 2) - ($(cards).width() / 2);
            var relY = $(table).offset().top - $(cards).offset().top + ($(table).height() / 2) - ($(cards).width() / 2) - 160;

            $(this).toggleClass('folded');

            if ($(this).hasClass('folded')) {
                cards.animate({
                    left: $(cards).position().left + relX, 
                    top: $(cards).position().top + relY
                }, function () {
                    $(cards).fadeOut(function () { $(this).remove(); });
                });
            }
        });*/

        $('.table-cards').click(function () {
            $(this).toggleClass('showdown');
        });

        $('.game-ui button').click(function () {
            $(this).parent().toggleClass('hide');
        });

        $('.chips').click(function () {
            function arrangeChips(el, amount) {
                // round to fifties
                amount = Math.round(amount / 50) * 50;

                $chips = $(el);
                $chips.empty();

                var limit = 0;

                while (amount > 0 )
                {
                    var chips = [
                        ['black', 200],
                        ['blue', 100],
                        ['red', 50]
                    ];

                    $.each(chips, function (i, c) {
                        var name = c[0];
                        var value = c[1];

                        if (amount >= value) {
                            // create the chip
                            var chip = '<div class="chip chip-' + name + '"></div>';

                            // find a stack
                            var stack = $chips.find('.stack.stack-' + name + ':not(.stack-full):first');

                            if (!stack.length) {
                                // no stack, we gotta create it
                                $chips.append('<div class="stack stack-' + name + '"></div>');

                                // todo: DRY (do {} while {}?)
                                stack = $chips.find('.stack.stack-' + name + ':not(.stack-full):first')[0];
                            }

                            // should always be just one element
                            stack = $(stack);

                            // lets put the chip into that stack
                            stack.append(chip);

                            // check if we exceeded the stack limit
                            if (stack.find('.chip').length > 6)
                                stack.addClass('stack-full');

                            // chip created, lets create some more if needed
                            amount -= value;
                            
                            return false;
                        }
                    });
                }

                if ($chips.parents('.table-space').hasClass('reverse'))
                    $chips.attr('dir', 'rtl');
            }

            var min = 1500;
            var max = 20000;
            var x = Math.floor(Math.random() * (max - min + 1) + min);

            arrangeChips($(this), x);
        })
    });
}

app.config(['$routeProvider', function($routeProvider) {
    $routeProvider.when('/', {templateUrl: 'static/templates/login.html', controller: LoginController}).
    when('/browser', {templateUrl: 'static/templates/browser.html', controller: BrowserController}).
    when('/room/:id', {templateUrl: 'static/templates/room.html', controller: RoomController}).
    otherwise({redirectTo: '/'});
}]);