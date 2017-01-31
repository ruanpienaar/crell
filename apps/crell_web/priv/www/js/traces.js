(function(){
    var app = angular.module('CrellApp', []);
    app.controller('CrellController', function($scope, $http){


    var url = window.location.href;
    var arr = url.split("/");

    // Goanna Ws Api
    var host = "ws://"+arr[2]+"/goanna_api/ws";
    var gws = new WebSocket(host);
    gws.onopen = function(){
        //message('onopen : '+socket.readyState+' (open)');
        // gws.send(JSON.stringify({'get':'nodes'}));
        gws.send(JSON.stringify({'get':'active_traces'}));
    }

    gws.onmessage = function(msg){

        // only allow the rest, if there are nodes...
        // gws.send(JSON.stringify({'get':'active_traces'}));
        // gws.send(JSON.stringify({'get':'runtime_modules'}));

        if(msg.data=='ok'){
            // why did we return ok anyways???...
        }else{
            var json_data = JSON.parse(msg.data);

            if(json_data.hasOwnProperty('traces')) {
                for(var i = 0; i < json_data['traces'].length; i++) {
                    var obj = json_data['traces'][i];
                    process_trace_obj(obj);
                }
            } else if(json_data.hasOwnProperty('active_traces')) {
                $('#trace_patterns').empty();
                for(var i = 0; i < json_data['active_traces'].length; i++) {
                    var obj = json_data['active_traces'][i];
                    var mfastr = obj['module']+':'+obj['function']+'/'+obj['arity'];
                    $('#trace_patterns').append('<option value='+mfastr+'>'+mfastr+'</option>');
                }
            } else if(json_data.hasOwnProperty('runtime_modules')) {
                $('#trace_mod').empty();
                $('#trace_mod').append('<option value="*" >*</option>');
                for(var i = 0; i < json_data['runtime_modules'].length; i++) {
                    var obj = json_data['runtime_modules'][i];
                    $('#trace_mod').append('<option value='+obj+' >'+obj+'</option>')
                }
            } else if(json_data.hasOwnProperty('functions')) {
                $('#trace_fun').empty();
                $('#trace_fun').append('<option value="*" >*</option>');
                for(var i = 0; i < json_data['functions'].length; i++) {
                    var obj = json_data['functions'][i];
                    $('#trace_fun').append('<option value='+obj+' >'+obj+'</option>')
                }
            } else if(json_data.hasOwnProperty('traces')) {
                for(var i = 0; i < json_data['traces'].length; i++) {
                    var obj = json_data['traces'][i];

                }
            } else if(json_data.hasOwnProperty('traces_polling_end')) {
                $('#pollBtn').attr("disabled", false);
                $('#stoppollBtn').attr("disabled", true);
                $('#loaderImg').attr("class", "invisible");
            }
        }
    }

    gws.onclose = function(){
        ws.send(JSON.stringify({'module':'crell_server',
                                'function':'nodes',
                                'args':
                                    []
                               })
        );
    }

    // crell ws api
    var ws_url = "ws://"+arr[2]+"/crell/ws";
    var ws = new WebSocket(ws_url);
    ws.onopen = function(){
        // ws.send(JSON.stringify({'module':'crell_server',
        //                         'function':'nodes',
        //                         'args':
        //                             []
        //                        })
        // );
        ws.send(JSON.stringify({'module':'crell_server',
                                'function':'is_tracing',
                                'args':
                                    []
                               })
        );
    }

    ws.onmessage = function(msg){
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
            }
        } else if(json_data.hasOwnProperty('is_tracing')) {
            if(json_data.is_tracing == 'true'){
                $('#enable_trc_btn').attr("disabled", true);
                $('#disable_trc_btn').attr("disabled", false);
            } else if(json_data.is_tracing == 'false') {
                $('#enable_trc_btn').attr("disabled", false);
                $('#disable_trc_btn').attr("disabled", true);
            }
        }
    }
    ws.onclose = function(){

    }

    $('#pollBtn').click(function(){
        $('#pollBtn').attr("disabled", true);
        $('#stoppollBtn').attr("disabled", false);
        $('#loaderImg').attr("class", "visible");
        gws.send( JSON.stringify({"polling":"true"}) );
    });

    $('#stoppollBtn').click(function(){
        gws.send( JSON.stringify({"polling":"false"}) );
    });

    $('#fetchBtn').click(function(evt){
        gws.send( JSON.stringify( {"fetch":$('#fetchSelect').val()} ));
    });

    $('#traceBtn').click(function(evt){
        var trace_json = {
            'mod':$('#trace_mod').val(),
            'fun':$('#trace_fun').val(),
            'ara':$('#trace_ara').val(),
            'tim':$('#trace_tim').val(),
            'mes':$('#trace_mes').val()
        };
        gws.send( JSON.stringify( {'trace': trace_json } ));
    });

    $('#disconnectBtn').click(function(){
        var ans=confirm('Do you want to disconnect '+$('#nodes option:selected').val());
        if(ans){
            gws.send(JSON.stringify({'module': 'goanna_api',
                                        'function': 'remove_node',
                                        'args':
                                            [$('#nodes option:selected').val()]
                                            //'['+$('#nodes option:selected').val()+']'
                                       })
            );
        }
    });

    // $('#addNodeBtn').click(function(){
    //     gws.send(JSON.stringify({'module':'goanna_api',
    //                                 'function':'add_node',
    //                                 'args':
    //                                     [$('#add_node').val(),
    //                                      $('#add_cookie').val(),
    //                                      $('#add_type').val()]
    //                                })
    //     );
    // });

    $('#trace_mod').change(function(){
        gws.send(JSON.stringify({'module':'crell_server',
                                    'function':'runtime_module_functions',
                                    'args':
                                        [$('#trace_mod').val()]
                                   })
        );
    });

    $('#stopTracePatBtn').click(function(){
        gws.send(JSON.stringify({'module':'goanna_api',
                                    'function':'stop_trace',
                                    'args':
                                        [$('#trace_patterns').val()]
                                   })
        );
    });

    $('#stopTraceBtn').click(function(){
        gws.send( JSON.stringify({"polling":"false"}) );
        gws.send(JSON.stringify({'module':'goanna_api',
                                    'function':'stop_trace',
                                    'args':
                                        []
                                   })
        );
    });

    $('#enable_trc_btn').click(function(){
        ws.send(JSON.stringify({'module':'crell_server',
                                'function':'toggle_tracing',
                                'args':
                                    []
                               })
        );
    });
    $('#disable_trc_btn').click(function(){
        ws.send(JSON.stringify({'module':'crell_server',
                                'function':'toggle_tracing',
                                'args':
                                    []
                               })
        );
    });

// function message(string){
//     console.log(string);
// }

    function process_trace_obj(obj){
        var table_row='';
        switch (obj['type']) {
            case 'trace':
                table_row=
                '<tr>'+
                ' <th scope="row">'+obj['datetime']+'</th>'+
                ' <td>'+obj['pid']+'</td>'+
                ' <td>'+obj['label']+'</td>'+
                ' <td><pre><code class="erlang">'+obj['info']+'</code></pre></td>'+
                '</tr>';
                $('#traces_table').append(table_row);
                break;
            case 'trace_extra':
                table_row=
                '<tr>'+
                ' <th scope="row">'+obj['datetime']+'</th>'+
                ' <td>'+obj['pid']+'</td>'+
                ' <td>'+obj['label']+'</td>'+
                ' <td><pre><code class="erlang">'+obj['info']+'</code></pre></td>'+
                ' <td><pre><code class="erlang">'+obj['extra']+'</code></pre></td>'+
                '</tr>';
                $('#traces_table').append(table_row);
                break;
            case 'drop':
                console.log('Dropped '+obj['dropped']+' trace msgs.');
                break;
        }
    }

  });
})();