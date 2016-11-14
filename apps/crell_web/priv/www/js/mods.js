
(function(){
  var app = angular.module('CrellApp', []);
  app.controller('CrellController', function($scope, $http){
    // first run
    $scope.mods = {};
    $scope.mod_traced = 'test';
    var all_mods = $http.get("/crell_mod/all");
    all_mods.success(function(data, status, headers, config) {
        $scope.mods = data;
    });
    all_mods.error(function(data, status, headers, config) {
        alert("rest call failed!");
    });

    $scope.trace_module = function(module, checked){
        var restUrl="";
        
        alert($scope.mod_traced);
        $scope.mod_traced = module;
        
        if($scope.mod_traced == ''){
          $scope.mod_traced = module;
          restUri = "trace/";
        } else {
          $scope.mod_traced = '';
          restUri = "remove_trace/";
        }
        
        var response = $http.get("/crell_mod/" + restUri + module);
        response.success(function(data, status, headers, config) {
            // here, do something with data..........
            // show the trace screen.......
        });
        response.error(function(data, status, headers, config) {
           alert("rest call failed!");
        });
    }

  });
})();