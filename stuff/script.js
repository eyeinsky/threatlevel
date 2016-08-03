var id = -1
var getId = function() {
  id++
  return id
}


function point (fn) {
  var id = getId()
  var observers = []
  var add = function(listener) {
    console.log(listener.id, 'added')
    observers.push(listener)
  }
  var remove = function(listener) {
    var i = observers.indexOf(listener)
    if (i >= 0) {
      var removed = observers.splice(i, 1)
      console.log(listener.id, 'removed')
      return true
    }else {
      console.log(listener.id, 'not found')
      return false
    }
  }
  var once = function(listener) {
    var send0 = listener.send
    var wrap = function(ev) {
      send0(ev)
      return remove(listener)
    }
    listener.send = wrap
    add(listener)
  }
  var send0 = function(ev) {
    var len = observers.length
    for (var i = 0; i < len; i++) {
      observers[i].send(ev)
    }
  }
  var send = typeof fn === 'function'
    ? function(ev) { fn(ev); console.log(id, 'executed'); send0(ev) }
    : send0
  return {
    add: add,
    once: once,
    remove: remove,
    send: send,
    observers: observers,
    id: id
  }
}

var p1 = point(function() {
  console.log('jee');
})

var sleep = function(s) {
  var e = new Date().getTime() + (s * 1000);
  while (new Date().getTime() <= e) { ; }
}
var usleep = function(s) {
  var e = new Date().getTime() + (s / 1000);
  while (new Date().getTime() <= e) { ; }
}

var p2 = point(function () {
  var d = new Date()
  document.getElementById('o1').innerHTML = d.toString()
})

var p3 = point(function () {
  var d = new Date()
  document.getElementById('o2').innerHTML = d.toString()
})

p1.add(p2)
p1.once(p3)
