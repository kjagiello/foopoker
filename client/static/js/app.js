var app = angular.module('foopoker', []);
 
app.factory('user', function ($rootScope) {
    var userService = {};

    userService.username = '';

    return userService;
});

app.factory('socket', function ($rootScope) {
    var host = "ws://localhost:9001/";
    var socket = new WebSocket(host);

    socket.onopen = function (){
        console.log(socket.readyState + ': open')
    }
    
    socket.onerror = function (e){
        console.log(e); 
    }

    socket.onclose = function(){
        console.log(socket.readyState + ': closed')
    }

    return {
        on: function (eventName, callback) {
            socket.onmessage = function(){
                var args = arguments;
                $rootScope.$apply(function() {
                    callback.apply(socket, args);
                });
            }
        },

        emit: function (eventName, data) {
            socket.send(data);
        }
    };
});

function ChatController($scope, socket, user) {
    $scope.messages = [];
    $scope.maxLines = 50;

    socket.on('a', function (msg) {
        var data = JSON.parse(msg.data);
        $scope.addLine(data.username, data.message);

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
        socket.emit('chat', JSON.stringify({username: user.username, message: $scope.message}));
        
        $scope.addLine(user.username, $scope.message);
        $scope.message = '';
    }
}

function LoginController($scope, user) {
    $scope.login = function () {
        user.username = prompt('Choose an username');
        $('#login-interface').hide();
        $('#game-interface').show();
    }
}