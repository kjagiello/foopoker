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

app.factory('user', function ($rootScope, $location) {
    var userService = {};

    userService.username = '';
    userService.room = null;
    userService.isInRoom = function () {
        $location.path().lastIndexOf('/room/', 0) === 0
    }

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
                user.id = message.id;

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

function GameController ($scope, $element, user, socket) {
    $scope.user = user;
    $scope.betAmount = 0;
    $scope.bestHand = null;
    $scope.raise = false;

    socket.on('cleanup', function (data) {    
        $scope.betAmount = 0;
        $scope.bestHand = null;
        $scope.raise = false;
    });

    socket.on('update_money', function (data) {
        user.wallet = data.money;
    });

    socket.on('best_hand', function (data) {
        $scope.bestHand = data.hand;
    })

    $scope.$on('$viewContentLoaded', function(){
        $($element).find('.slider').slider({
            slide: function (event, ui) {
                $scope.$apply(function (){
                    $scope.betAmount = ui.value;
                });
            }
        });
    });

    $scope.betFold = function () {
        socket.emit('command', {name: 'fold', arguments: ""});
    }

    $scope.betCall = function () {
        socket.emit('command', {name: 'call', arguments: ""});
    }

    $scope.betRaise = function () {
        $scope.raise = !$scope.raise;
        $($element).find('.slider').slider("option", "min", user.betData.min);
        $($element).find('.slider').slider("option", "max", user.wallet);
    }

    $scope.betRaiseDo = function () {
        socket.emit('command', {name: 'raise', arguments: "" + $scope.betAmount});
        $scope.raise = false;
    }

    $scope.getUp = function () {
        socket.emit('command', {name: 'getup', arguments: ""});
    }
}

function BrowserController($scope, $location, socket, user) {
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

        socket.emit('leave_room', {});
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
                user.room = id;
                console.log(user);
            });
        });
    }
}

function SeatController($scope, $attrs, $timeout, $location, $element, socket, user) {
    $scope.seatId = $attrs.seatId;
    $scope.player = null;
    $scope.extraClasses = 'empty'; 
    $scope.bet = false;
    $scope.user = user;

    // $scope.avatar = 'http://2.bp.blogspot.com/-RatTLFiu6J4/T5l_v59jbVI/AAAAAAAAQ2A/kelVxm_vcLI/s400/blank_avatar_220.png';
    socket.on('user_join', function (data) {
        if (data.seat != $scope.seatId)
            return;

        $scope.player = data.user;

        console.log(user.id, $scope.player.id);

        if (user.id == $scope.player.id) {
            user.sitting = true;
        }
    });

    socket.on('user_leave', function (data) {
        if (data.seat != $scope.seatId)
            return;

        if (user.id == $scope.player.id) {
            user.sitting = false;
        }

        $scope.player = null;
    });

    socket.on('bet', function (data) {
        if (data.seat != $scope.seatId) {
            $scope.bet = false;
            return;
        }

        user.bet = user.id == $scope.player.id;
        $scope.bet = true;

        if (user.bet) {
            user.betData = {
                min: data.min
            }
        }

        $('body').find('.bet-timer').stop().width('100%');
        $($element).find('.bet-timer').width('100%');
        $($element).find('.bet-timer').animate({
            width: '0%'
        }, data.time * 1000);
    });

    socket.on('new_player_card', function (data) {
        if (data.seat != $scope.seatId)
            return;

        $scope.player.cards = $scope.player.cards || [];
        $scope.player.cards.push(data);

        console.log($scope.player);
    });

    socket.on('update_stake', function (data) {
        if (data.seat != $scope.seatId)
            return;

        $scope.player.stake = data.stake;
    });

    $scope.sitDown = function () {
        socket.emit('command', {name: 'sit', arguments: $scope.seatId});
    }
}

app.directive('seat', function () {
    return {
        templateUrl: 'static/templates/seat.html',
        restrict: 'E',
        scope: {
            seatId: '@'
        },
        controller: SeatController
    }
})

function RoomController($scope, socket, user) {
    $scope.cards = [];

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

        socket.emit('sync_board', {});
    });

    socket.on('new_card', function (data) {
        $scope.cards.push(data);
    });

    socket.on('cleanup', function (data) {    
        $scope.cards = [];
    });
}

app.config(['$routeProvider', function($routeProvider) {
    $routeProvider.when('/', {templateUrl: 'static/templates/login.html', controller: LoginController}).
    when('/browser', {templateUrl: 'static/templates/browser.html', controller: BrowserController}).
    when('/room/:id', {templateUrl: 'static/templates/room.html', controller: RoomController}).
    otherwise({redirectTo: '/'});
}]);