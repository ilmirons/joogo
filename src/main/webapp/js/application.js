$(function() {
  "use strict";

  var detect = $("#detect");
  var header = $('#header');
  var content = $('#content');
  var input = $('#input');
  var status = $('#status');
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
    content.html($('<p>', {
      text: 'Atmosphere connected using ' + response.transport
    }));
    input.removeAttr('disabled').focus();
    status.text('Choose name:');
    transport = response.transport;

    if (response.transport == "local") {
      subSocket.pushLocal("Name?");
    }
  };

  request.onReconnect = function(rq, rs) {
    socket.info("Reconnecting");
  };

  request.onMessage = function(rs) {

    // We need to be logged first.
    if (!myName) return;

    var message = rs.responseBody;
    try {
      var json = jQuery.parseJSON(message);
      console.log("got a message");
      console.log(json);
    } catch (e) {
      console.log('This doesn\'t look like a valid JSON object: ', message.data);
      return;
    }

    if (!logged) {
      logged = true;
      status.text(myName + ': ').css('color', 'blue');
      input.removeAttr('disabled').focus();
      subSocket.pushLocal(myName);
    } else {
      input.removeAttr('disabled');
      var me = json.author == author;
      var date = typeof(json.time) == 'string' ? parseInt(json.time) : json.time;
      addMessage(json.author, json.message, me ? 'blue' : 'black', new Date(date));
    }
  };

  request.onClose = function(rs) {
    console.log("closed");
  };

  request.onError = function(rs) {
    content.html($('<p>', {
      text: 'Sorry, but there\'s some problem with your ' + 'socket or the server is down'
    }));
  };

  subSocket = socket.subscribe(request);

  $(".board button").click(function() {
    var coords = $(this).attr("value").split(",");
    var x = parseInt(coords[0], 10);
    var y = parseInt(coords[1], 10);

    var json = {
      author: "defaultplayer",
      x: x,
      y: y
    };

    subSocket.push(jQuery.stringifyJSON(json));
  });

});