(function(){
    var app = angular.module('CrellApp', []);
    app.controller('CrellController', function($scope, $http){


    // 320 is the trace menu,
    // so screen_height - 320 - 50(padding)
    // resize the trace table...>
    // var wh = $(window).height() - 320 - 50;
    // $('traces_div').height(wh);
    // alert(wh);

    var url = window.location.href;
    var arr = url.split("/");

    // Goanna Ws Api
    var host = "ws://"+arr[2]+"/goanna_api/ws";
    var gws = new WebSocket(host);
    gws.onopen = function(){
    }

    gws.onmessage = function(msg){

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
                if( json_data['active_traces'].length > 0 ){
                    for(var i = 0; i < json_data['active_traces'].length; i++) {
                        var obj = json_data['active_traces'][i];
                        var mfastr = obj['module']+':'+obj['function']+'/'+obj['arity'];
                        $('#trace_patterns').
                            append('<option value='+mfastr+'>'+mfastr+'</option>');
                    }
                } else if ( json_data['active_traces'].length <= 0 ) {
                    stop_polling();
                }
            } else if(json_data.hasOwnProperty('functions')) {
                $('#trace_func').empty();
                $('#trace_func').append('<option value="*" >*</option>');
                for(var i = 0; i < json_data['functions'].length; i++) {
                    var obj = json_data['functions'][i];
                    $('#trace_func').append('<option value='+obj+' >'+obj+'</option>')
                }
            } else if(json_data.hasOwnProperty('traces')) {
                for(var i = 0; i < json_data['traces'].length; i++) {
                    var obj = json_data['traces'][i];

                }
            } else if(json_data.hasOwnProperty('traces_polling_end')) {
                if($('#disable_trc_btn').prop('disabled')){
                    disable_all_polling_input();
                } else {
                    disable_polling_input();
                }
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
        console.log('GWS Websocket closed !');
    }

    // crell ws api
    var ws_url = "ws://"+arr[2]+"/crell/ws";
    var ws = new WebSocket(ws_url);
    ws.onopen = function(){
        ws.send(JSON.stringify({'module':'crell_server',
                                'function':'nodes',
                                'args':
                                    []
                               })
        );
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
                $('#trace_patterns').attr("disabled", false);
                $('#trace_mod').attr("disabled", false);
                //$('#trace_func').attr("disabled", false);
                $('#trace_ara').attr("disabled", false);
                $('#trace_tim').attr("disabled", false);
                $('#trace_mes').attr("disabled", false);
                // $('#traceBtn').attr("disabled", false);
                $('#stopTracePatBtn').attr("disabled", false);
                $('#stopTraceBtn').attr("disabled", false);

                // tracing controls
                $('#pollBtn').attr("disabled", false);
                //$('#stoppollBtn').attr("disabled", false);
                $('#fetchBtn').attr("disabled", false);
                $('#fetchSelect').attr("disabled", false);
                $('#clearBtn').attr("disabled", false);
                $('#printBtn').attr("disabled", false);
                get_active_traces(),
                ws.send(JSON.stringify({
                    'module':'crell_server',
                    'function':'cluster_modules',
                    'args':
                        []
                    })
                );
                // gws.send(JSON.stringify({'get':'active_traces'}));
                // gws.send(JSON.stringify({'get':'runtime_modules'}));
            } else if(json_data.is_tracing == 'false') {
                gws.send( JSON.stringify({"polling":"false"}) );
                $('#enable_trc_btn').attr("disabled", false);
                $('#disable_trc_btn').attr("disabled", true);
                $('#trace_patterns').attr("disabled", true);
                $('#trace_mod').attr("disabled", true);
                $('#trace_func').attr("disabled", true);
                $('#trace_ara').attr("disabled", true);
                $('#trace_tim').attr("disabled", true);
                $('#trace_mes').attr("disabled", true);
                $('#traceBtn').attr("disabled", true);
                $('#stopTracePatBtn').attr("disabled", true);
                $('#stopTraceBtn').attr("disabled", true);

                // tracing controls
                $('#pollBtn').attr("disabled", true);
                //$('#stoppollBtn').attr("disabled", true);
                $('#fetchBtn').attr("disabled", true);
                $('#fetchSelect').attr("disabled", true);
                $('#clearBtn').attr("disabled", true);
                $('#printBtn').attr("disabled", true);
            }
        } else if(json_data.hasOwnProperty('mods')) {
            $('#trace_mod').empty();
            $('#trace_mod').append('<option value="*" >*</option>');
            for(var i = 0; i < json_data['mods'].length; i++) {
                var obj = json_data['mods'][i];
                $('#trace_mod').append('<option value='+obj+' >'+obj+'</option>');
            }
        } else if(json_data.hasOwnProperty('mod_funcs')) {
            $('#trace_func').empty();
            $('#trace_func').append('<option value="*" >*</option>');
            for(var i = 0; i < json_data['mod_funcs'].length; i++) {
                var obj = json_data['mod_funcs'][i];
                $('#trace_func').append('<option value='+obj+' >'+obj+'</option>');
            }
        }
    }
    ws.onclose = function(){
        console.log('Websocket closed !');
    }

    $('#pollBtn').click(function(){
        start_polling();
    });

    function start_polling(){
        $('#pollBtn').attr("disabled", true);
        $('#stoppollBtn').attr("disabled", false);
        $('#loaderImg').attr("class", "visible");
        $('#fetchBtn').attr("disabled", true);
        gws.send( JSON.stringify({"polling":"true"}) );
    }

    $('#stoppollBtn').click(function(){
        stop_polling();
    });

    function stop_polling(){
        gws.send( JSON.stringify({"polling":"false"}) );
    }

    $('#fetchBtn').click(function(evt){
        // gws.send( JSON.stringify( {"fetch":$('#fetchSelect').val()} ));
        gws.send(JSON.stringify({'module':'goanna_api',
                                 'function':'pull_all_traces',
                                 'args':
                                    [
                                     ]
                               })
        );
    });

    $('#traceBtn').click(function(evt){
        start_polling();
        gws.send(JSON.stringify({'module':'goanna_api',
                                 'function':'trace',
                                 'args':
                                    [$('#trace_mod').val(),
                                     $('#trace_func').val(),
                                     // $('#trace_ara').val(),
                                     $('#trace_tim').val(),
                                     $('#trace_mes').val()
                                     ]
                               })
        );

    });

    $('#printBtn').click(function(){
        window.print();
    });

    $('#clearBtn').click(function(){
        $('#traces_table').empty();
    });

    $('#stopTracePatBtn').click(function(){
        if($('#trace_patterns').val() != null){
            gws.send(JSON.stringify({'module':'goanna_api',
                                        'function':'stop_trace',
                                        'args':
                                            [$('#trace_patterns').val()]
                                       })
            );
        }
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
        toggle_tracing_ws_msg(ws)
    });
    $('#disable_trc_btn').click(function(){
        reset_inputs()
        toggle_tracing_ws_msg(ws)

    });

    function toggle_tracing_ws_msg(ws) {
        ws.send(JSON.stringify({'module':'crell_server',
                                'function':'toggle_tracing',
                                'args':
                                    [$('#nodes').val()]
                               })
        )
    }

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
                ' <td>'+obj.node+'</td>'+
                ' <td>'+obj['label']+'</td>'+
                ' <td><pre><code class="erlang">'+obj['info']+'</code></pre></td>'+
                '</tr>';
                $('#traces_table').append(table_row);
                break;
            case 'trace_extra':
                table_row=
                '<tr>'+
                ' <th scope="row">'+obj['datetime']+'</th>'+
                ' <td>'+obj.node+'</td>'+
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

    $('#trace_mod').change(function(){
        //get_functions($('#trace_mod').val());
        if($('#trace_mod').val() != "*"){
            $('#traceBtn').attr("disabled", false);
            $('#trace_func').attr("disabled", false);
            ws.send(JSON.stringify({'module':'crell_server',
                                    'function':'cluster_module_functions',
                                    'args':[$('#trace_mod').val()]
                                  }
            ));
        } else if($('#trace_mod').val() == "*"){
            $('#traceBtn').attr("disabled", true);
            $('#trace_func').attr("disabled", true);
        } else {
            alert('panic '+$('#trace_mod').val());
        }
    });

    $('#trace_pattern_refresh').click(function(){
        get_active_traces();
    });

    function disable_polling_input(){
        $('#pollBtn').attr("disabled", false);
        $('#stoppollBtn').attr("disabled", true);
        $('#fetchBtn').attr("disabled", false);
        $('#loaderImg').attr("class", "invisible");
    }

    function disable_all_polling_input(){
        $('#pollBtn').attr("disabled", true);
        $('#stoppollBtn').attr("disabled", true);
        $('#fetchBtn').attr("disabled", true);
        $('#loaderImg').attr("class", "invisible");
    }

    function reset_inputs(){
        $('#trace_mod').empty();
        $('#trace_mod').append('<option value="*" >*</option>');
        $('#trace_func').empty();
        $('#trace_func').append('<option value="*" >*</option>');
        $('#trace_tim').empty();
        $('#trace_mes').empty();
        $('#trace_patterns').empty();
    }

    function get_active_traces(){
        gws.send(JSON.stringify({
            'module':'goanna_api',
            'function':'list_active_traces',
            'args':
                []
            })
        );
    }

  });
})();