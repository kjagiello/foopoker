use "server.sml";

(* Setup some rooms *)
MLHoldemServer.createBoard ("FooBar 1", 8, (1, 2), (100, 1000));
MLHoldemServer.createBoard ("FooBar 2", 8, (5, 10), (100, 1000));
MLHoldemServer.createBoard ("FooBar High Rollers 1", 8, (50, 100), (100, 1000));
MLHoldemServer.createBoard ("FooBar High Rollers 2", 8, (50, 100), (100, 1000));
MLHoldemServer.createBoard ("FooBar High Rollers 3", 8, (50, 100), (100, 1000));
MLHoldemServer.createBoard ("FooBar High Rollers 4", 8, (50, 100), (100, 1000));
MLHoldemServer.createBoard ("FooBar V.I.P. only", 8, (500, 1000), (100, 1000));
MLHoldemServer.createBoard ("FooBar V.I.P. only", 8, (500, 1000), (100, 1000));

(* lets run it! *)
val fooServer = WebsocketServer.create (9001, MLHoldemServer.handleConnect, MLHoldemServer.handleDisconnect, MLHoldemServer.handleMessage, MLHoldemServer.tick);
PolyML.exception_trace(fn () => WebsocketServer.run fooServer);