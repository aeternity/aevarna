-module(aevarna).

-export([main/1]).

-define(OPT_SPEC,
    [ {src_file, undefined, undefined, string, "Varna source code file"}
    , {version, $V, "version", undefined, "Print compiler version"}
    , {verbose, $v, "verbose", undefined, "Verbose output"}
    , {help, $h, "help", undefined, "Show this message"}
    , {outfile, $o, "out", string, "Output file (experimental)"} ]).

usage() ->
    getopt:usage(?OPT_SPEC, "aevarna").

main(Args) ->
    case getopt:parse(?OPT_SPEC, Args) of
        {ok, {Opts, []}} ->
            case Opts of
                [version] ->
                    print_vsn();
                [help] ->
                    usage();
                _ ->
                    compile(Opts)
            end;

        {ok, {_, NonOpts}} ->
            io:format("Can't understand ~p\n\n", [NonOpts]),
            usage();

        {error, {Reason, Data}} ->
            io:format("Error: ~s ~p\n\n", [Reason, Data]),
            usage()
    end.


compile(Opts) ->
    case proplists:get_value(src_file, Opts, undefined) of
        undefined ->
            io:format("Error: no input source file\n\n"),
            usage();
        File ->
            compile(File, Opts)
    end.

compile(File, Opts) ->
    %% Verbose = proplists:get_value(verbose, Opts, false),
    OutFile = proplists:get_value(outfile, Opts, undefined),

    case aeva_compile:file(File, Opts) of
	{ok,Code,_Warnings} -> write_outfile(OutFile, Code);
	{error,Errors,_Warnings} ->
	    io:format("Errors:\n~p\n", [Errors])
    end.

write_outfile(undefined, _) -> ok;
write_outfile(Out, ResMap)  ->
    %% Lazy approach
    file:write_file(Out, term_to_binary(ResMap)),
    io:format("Output written to: ~s\n", [Out]).

print_vsn() ->
    {ok, Vsn} = aeva_compile:version(),
    io:format("Compiler version: ~s\n", [Vsn]).
