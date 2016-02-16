
(function(){
  var app = angular.module('CrellApp', []);
  app.controller('CrellController', function($scope, $http){
    // first run
    $scope.mods = {};
    var all_mods = $http.get("/crell_mod/all");
    all_mods.success(function(data, status, headers, config) {
        $scope.mods = data;
    });
    all_mods.error(function(data, status, headers, config) {
        alert("rest call failed!");
    });

    $scope.trace_module = function(module){
        // alert(module);
        alert(window.location.origin + module);

        // var response = $http.get(window.location.origin + id);
        // response.success(function(data, status, headers, config) {

        // });
        // response.error(function(data, status, headers, config) {
        //    alert("rest call failed!");
        // });
    }

  });
})();