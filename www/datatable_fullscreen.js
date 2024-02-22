// make datatable fullscreen
$(function () {
  'use strict';

  // If fullscreen is not supported jump right out
  if (!document.fullscreenEnabled) {
    return;
  }

  // get elements
  const fullscreenButton = $('#datatable_fullscreen');
  const datatable = $('#datatable')

  // make datatable go fullscreen when button is clicked
  fullscreenButton.on('click', function () {
    datatable[0].requestFullscreen();
    datatable.css({backgroundColor: "white", padding: "20px"});
  });
});