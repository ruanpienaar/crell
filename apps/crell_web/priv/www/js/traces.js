function message(string){
    console.log(string);
}

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

function connect(){
    try{
        var socket;
        var url = window.location.href;
        var arr = url.split("/");
        var host = "ws://"+arr[2]+"/goanna_api/ws";
        var socket = new WebSocket(host);

        socket.onopen = function(){
            //message('onopen : '+socket.readyState+' (open)');
            socket.send(JSON.stringify({'get':'nodes'}));
        }

        socket.onmessage = function(msg){
            if(msg.data=='ok'){
                // why did we return ok anyways???...
            }else{
                var json_data = JSON.parse(msg.data);

                if(json_data.hasOwnProperty('traces')) {
                    for(var i = 0; i < json_data['traces'].length; i++) {
                        var obj = json_data['traces'][i];
                        process_trace_obj(obj);
                    }
                } else if(json_data.hasOwnProperty('nodes')) {
                    if(json_data['nodes'].length == 0){
                        alert('First add a node.');
                        window.location.href = 'index.html'
                    } else {
                        // only allow the rest, if there are nodes...
                        socket.send(JSON.stringify({'get':'active_traces'}));
                        socket.send(JSON.stringify({'get':'runtime_modules'}));
                        $('#nodes').empty();
                        for(var i = 0; i < json_data['nodes'].length; i++) {
                            var obj = json_data['nodes'][i];
                            $('#nodes').append('<option value='+obj['node']+' >'+obj['node']+'</option>')
                        }
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

        socket.onclose = function(){
            //message('onclose : '+socket.readyState+' (Closed)');
            // Try and reconnect...
            // maybe create a sleep....
            // connect();
        }

        $('#pollBtn').click(function(){
            $('#pollBtn').attr("disabled", true);
            $('#stoppollBtn').attr("disabled", false);
            $('#loaderImg').attr("class", "visible");
            socket.send( JSON.stringify({"polling":"true"}) );
        });

        $('#stoppollBtn').click(function(){
            socket.send( JSON.stringify({"polling":"false"}) );
        });

        $('#fetchBtn').click(function(evt){
            socket.send( JSON.stringify( {"fetch":$('#fetchSelect').val()} ));
        });

        $('#traceBtn').click(function(evt){
            var trace_json = {
                'mod':$('#trace_mod').val(),
                'fun':$('#trace_fun').val(),
                'ara':$('#trace_ara').val(),
                'tim':$('#trace_tim').val(),
                'mes':$('#trace_mes').val()
            };
            socket.send( JSON.stringify( {'trace': trace_json } ));
        });

        $('#disconnectBtn').click(function(){
            var ans=confirm('Do you want to disconnect '+$('#nodes option:selected').val());
            if(ans){
                socket.send(JSON.stringify({'module': 'goanna_api',
                                            'function': 'remove_node',
                                            'args':
                                                [$('#nodes option:selected').val()]
                                                //'['+$('#nodes option:selected').val()+']'
                                           })
                );
            }
        });

        $('#addNodeBtn').click(function(){
            socket.send(JSON.stringify({'module':'goanna_api',
                                        'function':'add_node',
                                        'args':
                                            [$('#add_node').val(),
                                             $('#add_cookie').val(),
                                             $('#add_type').val()]
                                       })
            );
        });

        $('#trace_mod').change(function(){
            socket.send(JSON.stringify({'module':'crell_server',
                                        'function':'runtime_module_functions',
                                        'args':
                                            [$('#trace_mod').val()]
                                       })
            );
        });

        $('#stopTracePatBtn').click(function(){

            socket.send(JSON.stringify({'module':'goanna_api',
                                        'function':'stop_trace',
                                        'args':
                                            [$('#trace_patterns').val()]
                                       })
            );
        });

        $('#stopTraceBtn').click(function(){
            socket.send( JSON.stringify({"polling":"false"}) );
            socket.send(JSON.stringify({'module':'goanna_api',
                                        'function':'stop_trace',
                                        'args':
                                            []
                                       })
            );
        });

    } catch(exception) {
        message('!!! EXCEPTION: '+exception);
    }
}

// on document ready, connect the ws
$(document).ready(function() {
    if(!("WebSocket" in window)){
        $('<p>Oh no, you need a browser that supports WebSockets. How about <a href="http://www.google.com/chrome">Google Chrome</a>?</p>').appendTo('#container');
    }else{
       //The user has WebSockets
       connect();
    }

    $( "#clearBtn" ).click(function() {
        $('#traces_table').empty();
    });

    $('#printBtn').click(function(){
        window.print();
    });

});