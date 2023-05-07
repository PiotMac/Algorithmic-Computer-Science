function loadIMG(url, id){
  var P = new Promise( function (resolve, reject) {
      var parent = document.getElementById(id);
      var element = document.createElement("img");
      element.setAttribute("src", url);
      element.setAttribute("alt", url);
      parent.appendChild(element);
      element.onload  = function() { resolve(url); };
      element.onerror = function() { reject(url) ; };
    }
  );
  return P;
}

Promise.all([
    loadIMG("img/mountains1.jpg","pg"),
    loadIMG("img/mountains2.jpg","pg"),
    loadIMG("img/mountains3.jpg","pg"),
    loadIMG("img/mountains4.jpg","pg")
  ]).then(function() {
    console.log("Wszystko z równoległej się załadowało!");
  }).catch(function() {
    console.log("Błąd ładowania galerii rownoległej");
  });
