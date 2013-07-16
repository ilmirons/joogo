$ ->
  "use strict"
  gameId = $(".board input#game-id")
  gid = parseInt(gameId.attr("value"), 10)
  socket = $.atmosphere
  subSocket = undefined

  #  var transport = 'long-polling';
  transport = "websocket"
  request =
    url: "/play"
    contentType: "application/json"
    logLevel: "debug"
    transport: transport
    fallbackTransport: "long-polling"

  request.onOpen = (response) ->
    socket.info "Socket open"

  request.onReconnect = (rq, rs) ->
    socket.info "Reconnecting"

  request.onMessage = (rs) ->
    message = rs.responseBody
    try
      json = jQuery.parseJSON(message)
      board = json.board
      _(board.split("\n")).each (row, y) ->
        _(row.split("")).each (character, x) ->
          btn = $(".board .row-" + (y + 1) + " .col-" + (x + 1) + " button")
          if character is "+"
            btn.removeClass("b").removeClass("w").addClass "empty"
          else
            btn.removeClass("empty").addClass character
    catch e
      console.log "This doesn't look like a valid JSON object: ", message.data
      return

  request.onClose = (rs) ->
    console.log "closed"

  request.onError = (rs) ->
    console.log "error with socket or server down"

  subSocket = socket.subscribe(request)
  
  $(".board button").click ->
    coords = $(this).attr("value").split(",")
    x = parseInt(coords[0], 10)
    y = parseInt(coords[1], 10)
    
    json =
      type: "move"
      gameId: gid
      data:
        moveTo: [x, y]

    subSocket.push jQuery.stringifyJSON(json)

  $("#resign").click ->
    json =
      type: "resign"
      gameId: gid

    subSocket.push jQuery.stringifyJSON(json)

  $("#pass").click ->
    json =
      type: "pass"
      gameId: gid

    subSocket.push jQuery.stringifyJSON(json)