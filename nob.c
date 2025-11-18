#define NOB_IMPLEMENTATION
#include "nob.h"
#include <string.h>
#include <unistd.h> // for getpid on Linux

static void append_flags_from_file(Nob_Cmd *cmd, char *flag_file)
{
    Nob_String_Builder sb = {0};
    if (!nob_read_entire_file(flag_file, &sb))
    {
        nob_log(NOB_ERROR, "Failed to read flags from %s", flag_file);
        return;
    }
    nob_delete_file(flag_file);

    Nob_String_View sv = nob_sb_to_sv(sb);
    while (sv.count > 0)
    {
        Nob_String_View flag = nob_sv_chop_by_delim(&sv, ' ');
        flag = nob_sv_trim(flag);
        if (flag.count > 0)
	{
            const char *flag_str = nob_temp_sv_to_cstr(flag);
            nob_cmd_append(cmd, flag_str);
        }
    }
    nob_sb_free(sb);
}

const int backtick(Nob_Cmd *cmd, Nob_Cmd *append_to_command)
{
    char *tmp_output_file = nob_temp_sprintf("nob_backtick_output.tmp");
    if (!nob_cmd_run(cmd, .stdout_path = tmp_output_file))
    {
        nob_log(NOB_ERROR, "Failed to run %s", cmd->items[0]);
        return 0;
    }

    Nob_String_Builder sb = {0};
    if (!nob_read_entire_file(tmp_output_file, &sb))
    {
        nob_log(NOB_ERROR, "Failed to read from %s", tmp_output_file);
        return 0;
    }
    nob_delete_file(tmp_output_file);

    Nob_String_View sv = nob_sb_to_sv(sb);
    while (sv.count > 0)
    {
        Nob_String_View flag = nob_sv_chop_by_delim(&sv, ' ');
        flag = nob_sv_trim(flag);
        if (flag.count > 0)
	{
            const char *flag_str = nob_temp_sv_to_cstr(flag);
            nob_cmd_append(append_to_command, flag_str);
        }
    }
    nob_sb_free(sb);

    return 1;
}

int main(int argc, char **argv)
{
    NOB_GO_REBUILD_URSELF(argc, argv);

    if (argc > 1 && nob_sv_eq(nob_sv_from_cstr(argv[1]), nob_sv_from_cstr("clean")))
    {
        Nob_Cmd clean_cmd = {0};
        nob_cmd_append(&clean_cmd, "rm", "-rf", "lisp-sdl3");
        if (!nob_cmd_run(&clean_cmd))
	{
            return 1;
        }
        return 0;
    }

    Nob_Cmd xx = {0};
    Nob_Cmd yy = {0};
    //nob_cmd_append(&xx, "pkg-config", "--libs", "sdl3");
    nob_cmd_append(&xx, "pkg-config", "-libs", "sdl3");
    backtick(&xx, &yy);
    for (int i = 0 ; i < yy.count; i++)
    {
	    char *p = yy.items[i];

	    printf("%d %s\n", i, p);

    }


    // Build
    char* sdl_extra_cflags = "-ISDL_ttf/include";

    // Get SDL_CFLAGS from pkg-config
    Nob_Cmd pkg_cflags_cmd = {0};
    nob_cmd_append(&pkg_cflags_cmd, "pkg-config", "--cflags", "sdl3");
    char *cflags_tmp = nob_temp_sprintf("sdl_cflags.tmp");
    if (!nob_cmd_run(&pkg_cflags_cmd, .stdout_path = cflags_tmp))
    {
        nob_log(NOB_ERROR, "Failed to run pkg-config --cflags sdl3");
        return 1;
    }

    // Get SDL libdir
    Nob_Cmd pkg_libdir_cmd = {0};
    nob_cmd_append(&pkg_libdir_cmd, "pkg-config", "--variable=libdir", "sdl3");
    char *libdir_tmp = nob_temp_sprintf("sdl_libdir.tmp");
    if (!nob_cmd_run(&pkg_libdir_cmd, .stdout_path = libdir_tmp))
    {
        nob_log(NOB_ERROR, "Failed to run pkg-config --variable=libdir sdl3");
        return 1;
    }

    Nob_String_Builder libdir_sb = {0};
    if (!nob_read_entire_file(libdir_tmp, &libdir_sb))
    {
        nob_log(NOB_ERROR, "Failed to read libdir from %s", libdir_tmp);
        return 1;
    }

    const char *sdl_libdir = nob_temp_sv_to_cstr(nob_sb_to_sv(libdir_sb));
    nob_sb_free(libdir_sb);
    nob_delete_file(libdir_tmp);


    Nob_Cmd cmd = {0};
    nob_cmd_append(&cmd, "gcc", "-g");
    append_flags_from_file(&cmd, cflags_tmp);
    nob_cmd_append(&cmd, sdl_extra_cflags);

    // Input
    nob_cmd_append(&cmd, "lisp-sdl3.c");

    // Output
    nob_cmd_append(&cmd, "-o", "lisp-sdl3");

    nob_cmd_append(&cmd, "-lm");

    char* readline_ldflags = "-lreadline";
    if (strlen(readline_ldflags) > 0) {
        // Split and append if multi-arg
        Nob_String_View rl_ld_sv = nob_sv_from_cstr(readline_ldflags);
        while (rl_ld_sv.count > 0)
	{
            Nob_String_View flag = nob_sv_chop_by_delim(&rl_ld_sv, ' ');
            flag = nob_sv_trim(flag);
            if (flag.count > 0)
	    {
                const char *flag_str = nob_temp_sv_to_cstr(flag);
                nob_cmd_append(&cmd, flag_str);
            }
        }
    }

    const char* sdl_ldflags = nob_temp_sprintf("-L%s -lSDL3 -LSDL_ttf/build -lSDL3_ttf -Wl,-rpath,SDL_ttf/build", sdl_libdir);
    if (strlen(sdl_ldflags) > 0)
    {
        Nob_String_View sdl_ld_sv = nob_sv_from_cstr(sdl_ldflags);
        while (sdl_ld_sv.count > 0)
	{
            Nob_String_View flag = nob_sv_chop_by_delim(&sdl_ld_sv, ' ');
            flag = nob_sv_trim(flag);
            if (flag.count > 0)
	    {
                const char *flag_str = nob_temp_sv_to_cstr(flag);
                nob_cmd_append(&cmd, flag_str);
            }
        }
    }

    if (!nob_cmd_run(&cmd))
    {
        return 1;
    }

    return 0;
}

