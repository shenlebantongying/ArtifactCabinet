#include <clocale>
#include <cstdio>
#include <fstream>
#include <iostream>
#include <notcurses/notcurses.h>
#include <unistd.h>

int main()
{
    setlocale(LC_ALL, "");
    notcurses_options ncopt;

    memset(&ncopt, 0, sizeof(ncopt));

    struct notcurses* nc = notcurses_init(&ncopt, stdout);
    struct ncplane* stdplane = notcurses_stdplane(nc);


    unsigned evMask;

    nccell mync;
    nccell_init(&mync);
    nccell_set_fg_rgb(&mync, 0x18ce40);
    nccell_load_char(stdplane, &mync, 'h');

    ncplane_hline(stdplane, &mync, 5);
    ncplane_vline(stdplane, &mync, 5);

    for (int i = 0; i < 30; i++) {
        for (int j = 0; j < 30; j++) {

            // ncplane_putchar(stdplane,'\n');
            // ncplane_putchar_yx(stdplane, i, j, '*');
            notcurses_render(nc);
            usleep(5000);
        }
    }

    notcurses_cursor_enable(nc,2,2);

    ncinput * ni;

    notcurses_mice_enable(nc,NCMICE_ALL_EVENTS);

    std::ofstream out("filename.txt", std::ios_base::out);

    std::cout<<"wtf";
    out<<"wtf" <<std::endl;
    notcurses_get_blocking(nc,ni);
    std::cout<<ni->x<< " " <<ni->y <<std::endl;

    out.close();

    notcurses_stop(nc);
    return 0;
}
