$(function() {
  "use strict";

  var gameId = $(".board input#game-id");
  var socket = $.atmosphere;
  var subSocket;
//  var transport = 'long-polling';
  var transport = 'websocket';

  var request = {
    url: "/the-chat",
    contentType: "application/json",
    logLevel: 'debug',
    transport: transport,
    fallbackTransport: 'long-polling'
  };

  request.onOpen = function(response) {
    transport = response.transport;

    if (response.transport == "local") {
      subSocket.pushLocal("Name?");
    }
  };

  request.onReconnect = function(rq, rs) {
    socket.info("Reconnecting");
  };

  request.onMessage = function(rs) {

    var message = rs.responseBody;
    try {
      var json = jQuery.parseJSON(message);
      console.log("got a message");
      console.log(json);
    } catch (e) {
      console.log('This doesn\'t look like a valid JSON object: ', message.data);
      return;
    }
  };

  request.onClose = function(rs) {
    console.log("closed");
  };

  request.onError = function(rs) {
    console.log("error with socket or server down");
  };

  subSocket = socket.subscribe(request);

  $(".board button").click(function() {
    var coords = $(this).attr("value").split(",");
    var gid = parseInt(gameId.attr("value"), 10);
    var x = parseInt(coords[0], 10);
    var y = parseInt(coords[1], 10);

    var json = {
      game: gid,
      x: x,
      y: y
    };

    subSocket.push(jQuery.stringifyJSON(json));
  });

});