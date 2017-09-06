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
                    get_db_tables(json_data.nodes[0]);
                }
            }
        } else if(json_data.hasOwnProperty('db_tables')) {
            // handle ets
            $('#ets_db_tables').empty();
            var db_tables = json_data.db_tables;
            for(var e in db_tables.ets_tables){
                etbl = db_tables.ets_tables[e];
                $('#ets_db_tables').append(
                    '<tr><td>'+ets_exp_chk(etbl)+'</td>'+
                    '<td>'+etbl+'</td></tr>');
            }

            // handle mnesia, if mnesia is started.
            $('#mnesia_db_tables').empty();
            if(db_tables.hasOwnProperty('mnesia_tables')){
                //alert(' has mnesia tables');
                var db_tables = json_data.db_tables;
                for(var m in db_tables.mnesia_tables){
                    mtbl = db_tables.mnesia_tables[m];
                    $('#mnesia_db_tables').append(
                        '<tr><td>'+mnesia_exp_chk(mtbl)+'</td>'+
                        '<td>'+mtbl+'</td></tr>');
                }
            } else {
                $('#mnesia_db_tables').append(
                    '<tr><td>N/A</td></tr>');
            }
        // {"ets_dl_url":"/tmp/bla"}
        } else if(json_data.hasOwnProperty('ets_dl_url')) {
            var dl = json_data.ets_dl_url;
            window.location = dl;
        } else if(json_data.hasOwnProperty('mnesia_dl_url')) {
            var dl = json_data.mnesia_dl_url;
            window.location = dl;
        }
    }

    function ets_exp_chk(ets_tbl_name){
        return '<input class="ets-form-check-input form-check-input"'+
        ' type="checkbox" name="ets_table" '+
        'value="'+ets_tbl_name+'" />';
    }

    function mnesia_exp_chk(mnesia_tbl_name){
        return '<input class="mnesia-form-check-input form-check-input"'+
        ' type="checkbox" name="mnesia_table" '+
        'value="'+mnesia_tbl_name+'" />';
    }

    // function export_ets_table_html_td(ets_tbl_name){
    //     var html = '<td><button onclick='+
    //     '"export_db_table(\''+ets_tbl_name+'\')">'+
    //     '"Export</button></td>';
    //     return html;
    // }

    function get_db_tables(node){
        ws.send(JSON.stringify({'module':'crell_server',
                                'function':'get_db_tables',
                                'args':
                                    [node]
                               })
        );
    }

    $('#get_db_tables').click(function(){
        get_db_tables($('#nodes').val());
    });

    $scope.angjs_ets_export_sel = function angjs_ets_export_sel(){
        var ets_tbl_names=[];
        $('.ets-form-check-input:checkbox:checked').each(function(){
            ets_tbl_names.push($(this).val());
        });
        if(ets_tbl_names.length>0){
            ws.send(JSON.stringify({
                'module':'crell_server',
                'function':'dump_ets_tables',
                'args':[$('#nodes').val(), ets_tbl_names]
            }));
        }
    }

    $scope.angjs_mnesia_export_sel = function angjs_mnesia_export_sel(){
        var mnesia_tbl_names=[];
        $('.mnesia-form-check-input:checkbox:checked').each(function(){
            mnesia_tbl_names.push($(this).val());
        });
        if(mnesia_tbl_names.length>0){
            ws.send(JSON.stringify({
                'module':'crell_server',
                'function':'dump_mnesia_tables',
                'args':[$('#nodes').val(), mnesia_tbl_names]
            }));
        }
    }

  });
})();

function ets_export_sel(){
    angular.element(document.getElementsByTagName('body')).scope().angjs_ets_export_sel();
}

function mnesia_export_sel(){
    angular.element(document.getElementsByTagName('body')).scope().angjs_mnesia_export_sel();
}

function ets_select_all(){
    $('.ets-form-check-input:checkbox').each(function(){
        $(this).attr('checked', true);
    });
}

// function ets_select_none(){
//     $('.ets-form-check-input:checkbox').each(function(){
//         $(this).attr('checked', false);
//     });
// }

function mnesia_select_all(){
    $('.mnesia-form-check-input:checkbox').each(function(){
        $(this).attr('checked', true);
    });
}

// function mnesia_select_none(){
//     $('.mnesia-form-check-input:checkbox').each(function(){
//         $(this).attr('checked', false);
//     });
// }