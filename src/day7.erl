-module(day7).

%% --- Day 7: No Space Left On Device ---

%% You can hear birds chirping and raindrops hitting leaves as the
%% expedition proceeds. Occasionally, you can even hear much louder
%% sounds in the distance; how big do the animals get out here,
%% anyway?

%% The device the Elves gave you has problems with more than just its
%% communication system. You try to run a system update:

%% $ system-update --please --pretty-please-with-sugar-on-top
%% Error: No space left on device

%% Perhaps you can delete some files to make space for the update?

%% You browse around the filesystem to assess the situation and save
%% the resulting terminal output (your puzzle input). For example:

%% $ cd /
%% $ ls
%% dir a
%% 14848514 b.txt
%% 8504156 c.dat
%% dir d
%% $ cd a
%% $ ls
%% dir e
%% 29116 f
%% 2557 g
%% 62596 h.lst
%% $ cd e
%% $ ls
%% 584 i
%% $ cd ..
%% $ cd ..
%% $ cd d
%% $ ls
%% 4060174 j
%% 8033020 d.log
%% 5626152 d.ext
%% 7214296 k

%% The filesystem consists of a tree of files (plain data) and
%% directories (which can contain other directories or files). The
%% outermost directory is called /. You can navigate around the
%% filesystem, moving into or out of directories and listing the
%% contents of the directory you're currently in.

%% Within the terminal output, lines that begin with $ are commands
%% you executed, very much like some modern computers:

%%     cd means change directory. This changes which directory is the
%%     current directory, but the specific result depends on the
%%     argument:

%%         cd x moves in one level: it looks in the current directory
%%         for the directory named x and makes it the current
%%         directory.

%%         cd .. moves out one level: it finds the directory that
%%         contains the current directory, then makes that directory
%%         the current directory.

%%         cd / switches the current directory to the outermost
%%         directory, /.

%%     ls means list. It prints out all of the files and directories
%%     immediately contained by the current directory:

%%         123 abc means that the current directory contains a file
%%         named abc with size 123.
%%         dir xyz means that the current directory contains a
%%         directory named xyz.

%% Given the commands and output in the example above, you can
%% determine that the filesystem looks visually like this:

%% - / (dir)
%%   - a (dir)
%%     - e (dir)
%%       - i (file, size=584)
%%     - f (file, size=29116)
%%     - g (file, size=2557)
%%     - h.lst (file, size=62596)
%%   - b.txt (file, size=14848514)
%%   - c.dat (file, size=8504156)
%%   - d (dir)
%%     - j (file, size=4060174)
%%     - d.log (file, size=8033020)
%%     - d.ext (file, size=5626152)
%%     - k (file, size=7214296)

%% Here, there are four directories: / (the outermost directory), a
%% and d (which are in /), and e (which is in a). These directories
%% also contain files of various sizes.

%% Since the disk is full, your first step should probably be to find
%% directories that are good candidates for deletion. To do this, you
%% need to determine the total size of each directory. The total size
%% of a directory is the sum of the sizes of the files it contains,
%% directly or indirectly. (Directories themselves do not count as
%% having any intrinsic size.)

%% The total sizes of the directories above can be found as follows:

%%     The total size of directory e is 584 because it contains a
%%     single file i of size 584 and no other directories.

%%     The directory a has total size 94853 because it contains files
%%     f (size 29116), g (size 2557), and h.lst (size 62596), plus
%%     file i indirectly (a contains e which contains i).

%%     Directory d has total size 24933642.

%%     As the outermost directory, / contains every file. Its total
%%     size is 48381165, the sum of the size of every file.

%% To begin, find all of the directories with a total size of at most
%% 100000, then calculate the sum of their total sizes. In the example
%% above, these directories are a and e; the sum of their total sizes
%% is 95437 (94853 + 584). (As in this example, this process can count
%% files more than once!)

%% Find all of the directories with a total size of at most
%% 100000. What is the sum of the total sizes of those directories?

-export([run/0
        ,part1/0
        ,part2/0
        ]).

-record(inode, {name, type, size=0, inodes = []}).

run() ->
    Input = input(),
    part1(Input),
    part2(Input).

part1() ->
    part1(input()).

part2() ->
    part2(input()).

%% not   84585
%% not 1155187
%% not 1324537
%% not 1492125
part1(FileSystem) ->
    DU = du(FileSystem),

    io:format("filesystem: ~p~n", [FileSystem]),
    io:format("du: ~p~n", [DU]),
    Total = lists:sum([Size || Size <- DU, Size =< 100000]),

    io:format("total of dirs under 100,000: ~p~n", [Total]).

part2(_Input) -> 'ok'.

du(#inode{size=Size, type='directory', inodes=Inodes}) ->
    du(Inodes, [Size]).

du(#inode{type='directory', inodes=Inodes}, Acc) ->
    du(Inodes, Acc);
du(Inodes, Acc) ->
    lists:foldl(fun du_fold/2, Acc, Inodes).

du_fold(#inode{type='directory', size=Size, inodes=Inodes}, Acc) ->
    du(Inodes, [Size | Acc]);
du_fold(#inode{}, Acc) -> Acc.

input() ->
    Bin = input:input(<<?MODULE_STRING>>),
    CLIOutput = binary:split(Bin, <<$\n>>, ['global', 'trim']),
    {_, FileSystem} = build_filesystem(CLIOutput),
    FileSystem.

build_filesystem(CLIOutput) ->
    build_filesystem(CLIOutput, {[<<"/">>], #inode{name = <<"/">>, type='directory'}}).

build_filesystem([<<"$ cd /">> | Rest], {_CWD, FileSystem}) ->
    build_filesystem(Rest, {[<<"/">>], FileSystem});
build_filesystem([<<"$ cd ..">> | Rest], {[_CWD | Path], FileSystem}) ->
    build_filesystem(Rest, {Path, FileSystem});
build_filesystem([<<"$ cd ", Directory/binary>> | Rest], {CWD, FileSystem}) ->
    build_filesystem(Rest, {[Directory | CWD], FileSystem});
build_filesystem([<<"$ ls">> | Rest], Acc) ->
    list_files(Rest, Acc);
build_filesystem([], Acc) -> Acc.

list_files([<<"$ ", _/binary>> | _]=Output, Acc) ->
    build_filesystem(Output, Acc);
list_files([<<"dir ", DirName/binary>> | Rest], Acc) ->
    list_files(Rest, add_directory(DirName, Acc));
list_files([File | Rest], Acc) ->
    list_files(Rest, add_file(File, Acc));
list_files([], Acc) -> Acc.

add_directory(DirName, {CWD, FileSystem}) ->
    NewFileSystem = add_inode(new_directory(DirName), lists:reverse(CWD), FileSystem),
    {CWD, NewFileSystem}.

new_directory(DirName) ->
    #inode{name=DirName, type='directory'}.

add_file(File, {CWD, FileSystem}) ->
    NewFileSystem = add_inode(new_file(File), lists:reverse(CWD), FileSystem),
    {CWD, NewFileSystem}.

new_file(File) ->
    [Size, Name] = binary:split(File, <<$\ >>),
    #inode{name=Name, type='file', size=binary_to_integer(Size, 10)}.

add_inode(#inode{size=Size}=Inode
         ,[]
         ,#inode{name=_Parent
                ,size=ParentSize
                ,inodes=Siblings
                }=ParentDir
         ) ->
    ParentDir#inode{size=ParentSize+Size
                   ,inodes=[Inode | Siblings]
                   };
add_inode(Inode, [Name | Descendants], #inode{name=Name}=CWD) ->
    add_inode(Inode, Descendants, CWD);
add_inode(Inode
         ,[Ancestor | Ancestors]
         ,#inode{name=_N
                ,size=CWDSize
                ,inodes=Inodes
                }=CWD
         ) ->
    %% io:format("going from ~s to ~s->~p: ~p~n", [_N, Ancestor, Ancestors, Inodes]),
    {'value', #inode{size=ASize, name=Ancestor, type='directory'}=NewCWD, AncestorSiblings}
        = lists:keytake(Ancestor, #inode.name, Inodes),

    #inode{size=NewASize}=NewAncestor = add_inode(Inode, Ancestors, NewCWD),
    CWD#inode{size=CWDSize - ASize + NewASize
             ,inodes=[NewAncestor | AncestorSiblings]
             }.
