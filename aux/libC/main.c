
#include "gui.h"
#include "jogo.h"

int main(int argc, char** argv){

	gui_init (argc, argv);
	jogo_init ();
	gui_start ();
	return 0;

}



