$( document ).ready(function() {
  $('p').each(function(index){
    var text = $(this).text();
    text = text.replace("@name (w+) ", "<h2>$1</h2>");
    text = text.replace("@title (w+) ", "<sub>$1</sub>");
    text = text.replace("@import (w+) ", "");
    text = text.replace("@export (w+) ", "");
    text = text.replace("@description (w+) ", "<p>$1</p>");
    text = text.replace("@param ")
  })
});
