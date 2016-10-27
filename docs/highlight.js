$(document).ready(function() {
  $('pre code').each(function(i, block) {
    // transfer language classes down to code element
    $(block).addClass($(block).parent().attr('class'));

    hljs.highlightBlock(block);
  });
});
