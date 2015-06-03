
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
        //alert("rest call failed!");
    });
  });
})();