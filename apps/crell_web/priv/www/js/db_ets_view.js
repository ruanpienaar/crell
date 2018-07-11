// read from qs table name
tbl = getUrlParameter('tbl');

(function(){
    var app = angular.module('CrellApp', []);
    app.controller('CrellController', function($scope, $http){

    $scope.nodes = {};
    var url = window.location.href;
    var arr = url.split("/");
    var ws_url = "ws://"+arr[2]+"/crell/ws";
    var ws = new WebSocket(ws_url);

    ws.onopen = function(){
        ws.send(JSON.stringify({'module':'crell_server',
                                'function':'nodes',
                                'args':
                                    []
                               })
        );
    }

    ws.onmessage = function(message){
        handle_message(message);
    }

    ws.onclose = function(){
        console.log('Websocket closed !');
    }

    function handle_message(msg){
        var json_data = JSON.parse(msg.data);
        if(json_data.hasOwnProperty('nodes')) {
            if(json_data.nodes.length == 0){
                alert('First add a node.');
                window.location.href = 'index.html'
            } else {
                $('#nodes').empty();
                for(var n in json_data.nodes){
                    var node = json_data.nodes[n];
                    $('#nodes').append('<option value='+node+' >'+node+'</option>');
                }
                // get the first Node's db tables'
                if(json_data.nodes.length>0){
                    get_db_tbl_content(json_data.nodes[0], tbl, 1);
                }
            }
        } else if(json_data.hasOwnProperty('ets_records')) {
            // pagination
            // NB: 50 is hardcoded here and in the backend as the max per page....
            $(function() {
                // todo:mmm what happens when we change the node?
                var node = $('#nodes').val();
                $('#pagination_area').pagination({
                    currentPage: json_data.PageNmr,
                    items: json_data.ets_records.count,
                    itemsOnPage: 50,
                    cssStyle: 'light-theme',
                    onPageClick: function(pageNumber, event){
                        //alert(pageNumber);
                        //angular.element(document.getElementsByTagName('body')).scope().angjs_fetch_page(pageNumber);
                        get_db_tbl_content(node, tbl, pageNumber);
                    }
                });
            });

            $('#record_table').empty();
            for(k in json_data.ets_records.records){
                var key = json_data.ets_records.records[k];
                $('#record_table').append('<tr><td>'+key+'</td></tr>');
            }
        }
    }

    $('#nodes').change(function(){
        var node = $('#nodes').val();
        get_db_tbl_content(node, tbl, 1);
    });

    // set table label
    $('#tbl_name').html('Table : '+tbl);

    // start with from = 0 , and to = max_rec_count
    function get_db_tbl_content(node, tbl, pageN){
        ws.send(JSON.stringify({
            'module':'crell_db',
            'function':'ets_entries_per_page',
            'args':
                [node, tbl, pageN]
        }));

    }

    $('#get_db_entries').click(function(){
        var node = $('#nodes').val();
        get_db_tbl_content(node, tbl);
    });

    // functions used externally
    $scope.angjs_fetch_page = function(page_nmr){
        var node = $('#nodes').val();
        get_db_tbl_content(node, tbl, page_nmr);
    }

  });
})();

