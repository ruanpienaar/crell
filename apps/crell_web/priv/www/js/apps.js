(function(){
  var app = angular.module('CrellApp', []);
  app.controller('CrellController', function($scope, $http){
    // first run
    $scope.app_data = {};

    var app_data_resp = $http.get("/crell_proc/apps");
    app_data_resp.success(function(data, status, headers, config) {
        //$scope.p_g_data.pasture_group = data.pasture_group;
        $scope.app_data = data;
    });
    app_data_resp.error(function(data, status, headers, config) {
        //alert("rest call failed!");
    });

    // Prev/Next click
    // $scope.p_g_data.scroll = function(id) {
    //     var response = $http.get(window.location.origin + id);
    //     response.success(function(data, status, headers, config) {
    //         $scope.p_g_data.prev          = data.prev;
    //         $scope.p_g_data.next          = data.next;
    //         $scope.p_g_data.pasture_group = data.pasture_group;
    //     });
    //     response.error(function(data, status, headers, config) {
    //         //alert("rest call failed!");
    //     });
    // }

  });
})();