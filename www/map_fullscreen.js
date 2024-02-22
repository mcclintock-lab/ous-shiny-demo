// make map fullscreen
$(function () {
  'use strict';

  // If fullscreen is not supported jump right out
  if (!document.fullscreenEnabled) {
    return;
  }

  // get elements
  const fullscreenButton = $('#map-fullscreen');
  const map = $('#map')

  // make map go fullscreen when button is clicked
  fullscreenButton.on('click', function () {
    map[0].requestFullscreen();
  });
});