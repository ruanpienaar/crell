(function(){
    var app = angular.module('CrellApp', []);
    app.controller('CrellController', function($scope, $http){
    // first run
    $scope.pid_data = {};

    var all_procs = $http.get("/crell_proc/pids/");
    all_procs.success(function(data, status, headers, config) {
        $scope.pid_data = data;
    });

    all_procs.error(function(data, status, headers, config) {
        alert("pid_data rest call failed!");
    });

    // $scope.trace_module = function(module){
    //     // var response = $http.get("/crell_mod/trace/" + module);
    //     // response.success(function(data, status, headers, config) {
    //     //     // here, do something with data..........
    //     //     // show the trace screen.......
    //     // });
    //     // response.error(function(data, status, headers, config) {
    //     //    alert("rest call failed!");
    //     // });
    // }



  });

})();




// {name: "init", children: [
        // {name: "erl_prim_loader", children: [
                //{name: "port 0", children: []}]}]}


// {"name":"user","children":[{"name":"user_drv","children":[
        /// {"name":"<0.32.0>","children":[]},
        /// {"name":"port 2944","children":[]}]}]}

// {"name":"<0.39.0>","children":[
        // {"name":"<0.33.0>","children":[
            // {"name":"<0.65.0>","children":[]}]}]}