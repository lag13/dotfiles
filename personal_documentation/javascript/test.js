'use strict';
// EXAMPLE FROM:
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise
var promiseCount = 0;

var thisPromiseCount = ++promiseCount;

var log = document.getElementById('log');
alert(thisPromiseCount +
') Started (<small>Sync code started</small>)<br/>');

// We make a new promise: we promise a numeric count of this promise, starting from 1 (after waiting 3s)
var p1 = new Promise(
	// The resolver function is called with the ability to resolve or reject
	// the promise. THIS FUNCTION IS CALLED IMMEDIATELY EVEN THOUGH RESOLVE AND
	// REJECT ARE NOT PROVIDED I'M NOT SURE HOW THAT WORKS. I think maybe the first time 
	function(r, reject) {
		alert(thisPromiseCount +
			') Promise started (<small>Async code started</small>)<br/>');
		// This is only an example to create asynchronism
		window.setTimeout(
			function() {
				// We fulfill the promise !
				r(thisPromiseCount);
			}, Math.random() * 2000 + 1000);
	}
);

// We define what to do when the promise is resolved/fulfilled with the then() call,
// and the catch() method defines what to do if the promise is rejected.
p1.then(
// Log the fulfillment value
function(val) {
	alert(val +
		') Promise fulfilled (<small>Async code terminated</small>)<br/>');
})
.catch(
// Log the rejection reason
function(reason) {
	console.log('Handle rejected promise ('+reason+') here.');
});

alert(thisPromiseCount +
') Promise made (<small>Sync code terminated</small>)<br/>');
