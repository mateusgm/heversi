
#include "gui.h"

//variaveis:

int podeClicar = TRUE;
int espacamento = LARGURA/8;
int numJogadores;
int corJogador;
int vez = PRETO;
int coordenadaX = -1;
int coordenadaY = -1;
int codTela = NUM_JOGADORES;
int coordenadaX_aux;
int coordenadaY_aux;
int help;

GLuint textura_tabuleiro;
GLuint textura_placar;
GLuint textura_popup;
GLuint textura_botao;


//init:

void gui_init (int argc, char** argv){

   if (argc > 1) help = 1;
   else help = 0;

	//inicializa glut:
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);

	//configurações de janela:
	glutInitWindowSize(LARGURA, ALTURA); 
	glutInitWindowPosition(POSICAO_JANELA);
	glutCreateWindow("UololoUthello");

	//cor de fundo:
	//glClearColor(COR_TABULEIRO_FUNDO);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glEnable(GL_TEXTURE_2D);
	glOrtho(0, LARGURA, 0, ALTURA, -1, 1);	

	//seta callbacks:
	glutDisplayFunc(display); //callback para display
	glutKeyboardFunc(keyboard); //callback para keyboard
	glutMouseFunc(mouse); //callback para mouse

}

void gui_start (void){

	glutMainLoop();

}

void gui_refresh (void){

	int resultado;
	if (coordenadaX != -1 || (numJogadores == 1 && vez != corJogador))
		resultado = uololouthello (coordenadaX, coordenadaY, numJogadores, corJogador, &vez);

	if(resultado > 0)
		codTela = FINAL;

	coordenadaX = coordenadaY = -1;
			
	if(numJogadores==2 || vez==corJogador)
		podeClicar = TRUE;
	else
		podeClicar = FALSE;

	if(numJogadores == 1 && vez != corJogador){
		display();
		sleep(1);
    	}else{
		glutPostRedisplay();
	}

	
}

//desenhaTabuleiro:
void desenhaTabuleiro(void){


	//desenha tabuleiro:

	glColor3f(1.0f, 1.0f, 1.0f); //seta cor.

	//carrega textura:
	textura_tabuleiro = SOIL_load_OGL_texture
		(
			"images/madeira.jpg",
			SOIL_LOAD_RGB,
			SOIL_CREATE_NEW_ID,
			SOIL_FLAG_MIPMAPS
		);
	if(0 == textura_tabuleiro){
		printf( "SOIL loading error: '%s'\n", SOIL_last_result() );
	}

	//pega textura:
	glBindTexture(GL_TEXTURE_2D, textura_tabuleiro);

	//amarra textura na tela:
	glBegin(GL_QUADS);
		glTexCoord2d(0.0, 0.0); glVertex2d(0.0, 0.0);
		glTexCoord2d(1.0, 0.0); glVertex2d(LARGURA, 0.0);
		glTexCoord2d(1.0, 1.0); glVertex2d(LARGURA, ALTURA-espacamento);
		glTexCoord2d(0.0, 1.0); glVertex2d(0.0, ALTURA-espacamento);
	glEnd();

	glDeleteTextures( 1, &textura_tabuleiro );


	//espaço do placar:

	glColor3f(1.0f, 1.0f, 1.0f); //seta cor.

	//carrega textura:
	textura_placar = SOIL_load_OGL_texture
		(
			"images/botao.jpg",
			SOIL_LOAD_RGB,
			SOIL_CREATE_NEW_ID,
			SOIL_FLAG_MIPMAPS
		);
	if(0 == textura_placar){
		printf( "SOIL loading error: '%s'\n", SOIL_last_result() );
	}

	//pega textura:
	glBindTexture(GL_TEXTURE_2D, textura_placar);

	//amarra textura na tela:
	glBegin(GL_QUADS);
		glTexCoord2d(0.0, 0.0); glVertex2d(0, ALTURA-espacamento);
		glTexCoord2d(1.0, 0.0); glVertex2d(LARGURA, ALTURA-espacamento);
		glTexCoord2d(1.0, 1.0); glVertex2d(LARGURA, ALTURA);
		glTexCoord2d(0.0, 1.0); glVertex2d(0, ALTURA);
	glEnd();

	glDeleteTextures( 1, &textura_placar );


	//linhas:
	glColor3f(COR_LINHAS); //seta cor.
	glLineWidth(4);
	glBegin(GL_LINES);
	int j;
	for(j=0; j<9; j++){
	  	glVertex2i(j*espacamento, 0);
	  	glVertex2i(j*espacamento, LARGURA);
	  	glVertex2i(0, LARGURA-j*espacamento);
	  	glVertex2i(LARGURA, LARGURA-j*espacamento);
	}
	  	glVertex2i(0, ALTURA);
	  	glVertex2i(LARGURA, ALTURA);
	glEnd();

	//borda:
	glColor3f(COR_LINHAS); //seta cor.
	glLineWidth(8);
	glBegin(GL_LINES);
	  	glVertex2i(0, 0);
	  	glVertex2i(LARGURA, 0);
	  	glVertex2i(LARGURA, 0);
	  	glVertex2i(LARGURA, ALTURA);
	  	glVertex2i(LARGURA, ALTURA);
	  	glVertex2i(0, ALTURA);
	  	glVertex2i(0, ALTURA);
	  	glVertex2i(0, 0);
	glEnd();

		
}

//desenhaPlacar:
void desenhaPlacar(void){

	//círculos do placar:

	drawCircle(LARGURA/4-espacamento/2, ALTURA-espacamento/2, RAIO, PRETO);
	drawCircle(3*LARGURA/4-espacamento/2, ALTURA-espacamento/2, RAIO, BRANCO);

	//linha divisória:
	glColor3f(COR_LINHAS); //seta cor.
	glLineWidth(4);
	glBegin(GL_LINES);
	  	glVertex2i(LARGURA/2, ALTURA);
	  	glVertex2i(LARGURA/2, ALTURA-espacamento);
	glEnd();

	//escreve na tela:
	
	glColor3f(COR_PLACAR); //cor

	char* aux = malloc(MAX_NUM * sizeof(char));
	//numPretas:
	sprintf(aux, "%d", qtdPretas);
	renderBitmapString(LARGURA/4+espacamento/2, ALTURA-espacamento/2-10, GLUT_BITMAP_TIMES_ROMAN_24, aux);
	//numBrancas:
	sprintf(aux, "%d", qtdBrancas);
	renderBitmapString(3*LARGURA/4+espacamento/2, ALTURA-espacamento/2-10, GLUT_BITMAP_TIMES_ROMAN_24, aux);

}

//desenhaPecas:
void desenhaPecas(void){

	int i, j;
	float aux1, aux2;
	char* aux = malloc(MAX_NUM * sizeof(char));
	
	for(i=0; i<8; i++){
		for(j=0; j<8; j++){
			if(tabuleiro[i][j] > 0){

				aux1 = mapeiaCoordXEmTela(i);
				aux2 = mapeiaCoordYEmTela(j);

				if(tabuleiro[i][j] == PRETO){

					drawCircle(aux1, aux2, RAIO, PRETO);

				}else if (tabuleiro[i][j] == BRANCO){

					drawCircle(aux1, aux2, RAIO, BRANCO);
				}else if (help){

					if(vez == BRANCO) glColor3f(1.0f,1.0f,1.0f);
					else glColor3f(0.0f,0.0f,0.0f);
					if (odds[i][j] > 0)sprintf(aux, "+%d", odds[i][j]);
					else sprintf(aux, "%d", odds[i][j]);
					renderBitmapString(aux1-espacamento/5, aux2-espacamento/8, GLUT_BITMAP_TIMES_ROMAN_24, aux);

				}
			}
		}
	}
	

}


//desenhaVencedor:
void desenhaVencedor(void){

	//desenha fundo:

	glColor3f(1.0f, 1.0f, 1.0f); //seta cor.

	//carrega textura:
	textura_popup = SOIL_load_OGL_texture
		(
			"images/popup.jpg",
			SOIL_LOAD_RGB,
			SOIL_CREATE_NEW_ID,
			SOIL_FLAG_MIPMAPS
		);
	if(0 == textura_popup){
		printf( "SOIL loading error: '%s'\n", SOIL_last_result() );
	}

	//pega textura:
	glBindTexture(GL_TEXTURE_2D, textura_popup);

	//amarra textura na tela:
	glBegin(GL_QUADS);
		glTexCoord2d(0.0, 0.0); glVertex2d(LARGURA/2-2*espacamento, ALTURA/2-espacamento);
		glTexCoord2d(1.0, 0.0); glVertex2d(LARGURA/2+2*espacamento, ALTURA/2-espacamento);
		glTexCoord2d(1.0, 1.0); glVertex2d(LARGURA/2+2*espacamento, ALTURA/2+espacamento);
		glTexCoord2d(0.0, 1.0); glVertex2d(LARGURA/2-2*espacamento, ALTURA/2+espacamento);
	glEnd();

	glDeleteTextures( 1, &textura_popup );

	//escreve resultado:
	glColor3f(COR_POPUP_TEXTO); //seta cor.
	if(qtdBrancas == qtdPretas){ //empate
		renderBitmapString(LARGURA/2-espacamento+18, ALTURA/2-espacamento/2+24, GLUT_BITMAP_TIMES_ROMAN_24, "Empatou!");
	}
	else if(qtdBrancas < qtdPretas){ //preto ganhou
		renderBitmapString(LARGURA/2-espacamento, ALTURA/2-espacamento/2+24, GLUT_BITMAP_TIMES_ROMAN_24, "Preto Ganhou!");
	}
	else{ //branco ganhou
		renderBitmapString(LARGURA/2-espacamento-8, ALTURA/2-espacamento/2+24, GLUT_BITMAP_TIMES_ROMAN_24, "Branco Ganhou!");
	}


}	

//desenha escolha do número de jogadores:
void desenhaEscolhaNumJogadores(void){


	//desenha fundo:

	glColor3f(1.0f, 1.0f, 1.0f); //seta cor.

	//carrega textura:
	textura_popup = SOIL_load_OGL_texture
		(
			"images/popup.jpg",
			SOIL_LOAD_RGB,
			SOIL_CREATE_NEW_ID,
			SOIL_FLAG_MIPMAPS
		);
	if(0 == textura_popup){
		printf( "SOIL loading error: '%s'\n", SOIL_last_result() );
	}

	//pega textura:
	glBindTexture(GL_TEXTURE_2D, textura_popup);

	//amarra textura na tela:
	glBegin(GL_QUADS);
		glTexCoord2d(0.0, 0.0); glVertex2d(LARGURA/2-3*espacamento, ALTURA/2-2*espacamento);
		glTexCoord2d(1.0, 0.0); glVertex2d(LARGURA/2+3*espacamento, ALTURA/2-2*espacamento);
		glTexCoord2d(1.0, 1.0); glVertex2d(LARGURA/2+3*espacamento, ALTURA/2+2*espacamento);
		glTexCoord2d(0.0, 1.0); glVertex2d(LARGURA/2-3*espacamento, ALTURA/2+2*espacamento);
	glEnd();

	glDeleteTextures( 1, &textura_popup );

	//botão1:


	//desenha fundo:

	glColor3f(1.0f, 1.0f, 1.0f); //seta cor.

	//carrega textura:
	textura_botao = SOIL_load_OGL_texture
		(
			"images/botao.jpg",
			SOIL_LOAD_RGB,
			SOIL_CREATE_NEW_ID,
			SOIL_FLAG_MIPMAPS
		);
	if(0 == textura_botao){
		printf( "SOIL loading error: '%s'\n", SOIL_last_result() );
	}

	//pega textura:
	glBindTexture(GL_TEXTURE_2D, textura_botao);

	//amarra textura na tela:
	glBegin(GL_QUADS);
		glTexCoord2d(0.0, 0.0); glVertex2d(LARGURA/2-espacamento, ALTURA/2+espacamento/2-espacamento/4);
		glTexCoord2d(1.0, 0.0); glVertex2d(LARGURA/2+espacamento, ALTURA/2+espacamento/2-espacamento/4);
		glTexCoord2d(1.0, 1.0); glVertex2d(LARGURA/2+espacamento, ALTURA/2+3*espacamento/2-espacamento/4);
		glTexCoord2d(0.0, 1.0); glVertex2d(LARGURA/2-espacamento, ALTURA/2+3*espacamento/2-espacamento/4);
	glEnd();


	//botão2:

	//pega textura:
	glBindTexture(GL_TEXTURE_2D, textura_botao);

	//amarra textura na tela:
	glBegin(GL_QUADS);
		glTexCoord2d(0.0, 0.0); glVertex2d(LARGURA/2-espacamento, ALTURA/2-espacamento/2+espacamento/4);
		glTexCoord2d(1.0, 0.0); glVertex2d(LARGURA/2+espacamento, ALTURA/2-espacamento/2+espacamento/4);
		glTexCoord2d(1.0, 1.0); glVertex2d(LARGURA/2+espacamento, ALTURA/2-3*espacamento/2+espacamento/4);
		glTexCoord2d(0.0, 1.0); glVertex2d(LARGURA/2-espacamento, ALTURA/2-3*espacamento/2+espacamento/4);
	glEnd();


	glDeleteTextures( 1, &textura_botao );

	//escreve na tela:
	glColor3f(COR_TITULO); //seta cor.
	renderBitmapString(LARGURA/2-espacamento, ALTURA-espacamento/2-8, GLUT_BITMAP_TIMES_ROMAN_24, "UololoUthello");


	//escreve na tela:
	glColor3f(COR_BOTAO_TEXTO); //seta cor.

	renderBitmapString(LARGURA/2-espacamento/2-14, ALTURA/2+espacamento-8-espacamento/4, GLUT_BITMAP_TIMES_ROMAN_24, "1 Jogador");
	renderBitmapString(LARGURA/2-espacamento/2-22, ALTURA/2-espacamento-8+espacamento/4, GLUT_BITMAP_TIMES_ROMAN_24, "2 Jogadores");


}

//desenha escolha da cor:
void desenhaEscolhaCor(void){


	//desenha fundo:

	glColor3f(1.0f, 1.0f, 1.0f); //seta cor.

	//carrega textura:
	textura_popup = SOIL_load_OGL_texture
		(
			"images/popup.jpg",
			SOIL_LOAD_RGB,
			SOIL_CREATE_NEW_ID,
			SOIL_FLAG_MIPMAPS
		);
	if(0 == textura_popup){
		printf( "SOIL loading error: '%s'\n", SOIL_last_result() );
	}

	//pega textura:
	glBindTexture(GL_TEXTURE_2D, textura_popup);

	//amarra textura na tela:
	glBegin(GL_QUADS);
		glTexCoord2d(0.0, 0.0); glVertex2d(LARGURA/2-3*espacamento, ALTURA/2-2*espacamento);
		glTexCoord2d(1.0, 0.0); glVertex2d(LARGURA/2+3*espacamento, ALTURA/2-2*espacamento);
		glTexCoord2d(1.0, 1.0); glVertex2d(LARGURA/2+3*espacamento, ALTURA/2+2*espacamento);
		glTexCoord2d(0.0, 1.0); glVertex2d(LARGURA/2-3*espacamento, ALTURA/2+2*espacamento);
	glEnd();

	glDeleteTextures( 1, &textura_popup );

	//botão1:

	glColor3f(1.0f, 1.0f, 1.0f); //seta cor.

	//carrega textura:
	textura_botao = SOIL_load_OGL_texture
		(
			"images/botao.jpg",
			SOIL_LOAD_RGB,
			SOIL_CREATE_NEW_ID,
			SOIL_FLAG_MIPMAPS
		);
	if(0 == textura_botao){
		printf( "SOIL loading error: '%s'\n", SOIL_last_result() );
	}

	//pega textura:
	glBindTexture(GL_TEXTURE_2D, textura_botao);

	//amarra textura na tela:
	glBegin(GL_QUADS);
		glTexCoord2d(0.0, 0.0); glVertex2d(LARGURA/2-espacamento, ALTURA/2+espacamento/2-espacamento/4);
		glTexCoord2d(1.0, 0.0); glVertex2d(LARGURA/2+espacamento, ALTURA/2+espacamento/2-espacamento/4);
		glTexCoord2d(1.0, 1.0); glVertex2d(LARGURA/2+espacamento, ALTURA/2+3*espacamento/2-espacamento/4);
		glTexCoord2d(0.0, 1.0); glVertex2d(LARGURA/2-espacamento, ALTURA/2+3*espacamento/2-espacamento/4);
	glEnd();


	//botão2:

	//pega textura:
	glBindTexture(GL_TEXTURE_2D, textura_botao);

	//amarra textura na tela:
	glBegin(GL_QUADS);
		glTexCoord2d(0.0, 0.0); glVertex2d(LARGURA/2-espacamento, ALTURA/2-espacamento/2+espacamento/4);
		glTexCoord2d(1.0, 0.0); glVertex2d(LARGURA/2+espacamento, ALTURA/2-espacamento/2+espacamento/4);
		glTexCoord2d(1.0, 1.0); glVertex2d(LARGURA/2+espacamento, ALTURA/2-3*espacamento/2+espacamento/4);
		glTexCoord2d(0.0, 1.0); glVertex2d(LARGURA/2-espacamento, ALTURA/2-3*espacamento/2+espacamento/4);
	glEnd();

	glDeleteTextures( 1, &textura_botao );

	//escreve na tela:
	glColor3f(COR_TITULO); //seta cor.
	renderBitmapString(LARGURA/2-espacamento, ALTURA-espacamento/2-8, GLUT_BITMAP_TIMES_ROMAN_24, "UololoUthello");


	drawCircle(LARGURA/2, ALTURA/2+espacamento-espacamento/4, RAIO, PRETO);
	drawCircle(LARGURA/2, ALTURA/2-espacamento+espacamento/4, RAIO, BRANCO);


}

//desenha símbolo de vez:
void desenhaVez(void){

	if(vez == PRETO){

		//seta para a esquerda:
		glColor3f(COR_VEZ_FUNDO); //seta cor.
		glBegin(GL_POLYGON);
		  	glVertex2i(LARGURA/2, ALTURA-espacamento/3+espacamento/4+2);
		  	glVertex2i(LARGURA/2, ALTURA-2*espacamento/3+espacamento/4+2);
		  	glVertex2i(LARGURA/2-espacamento/2, ALTURA-2*espacamento/3+espacamento/4+2);
		  	glVertex2i(LARGURA/2-espacamento/2, ALTURA-espacamento/3+espacamento/4+2);

		glEnd();

		//borda da seta para a esquerda:m
		glColor3f(COR_LINHAS); //seta cor.
		glLineWidth(2);
		glBegin(GL_LINE_STRIP);
		  	glVertex2i(LARGURA/2, ALTURA-espacamento/3+espacamento/4+2);
		  	glVertex2i(LARGURA/2, ALTURA-2*espacamento/3+espacamento/4+2);
		  	glVertex2i(LARGURA/2-espacamento/2, ALTURA-2*espacamento/3+espacamento/4+2);
		  	glVertex2i(LARGURA/2-espacamento/2, ALTURA-espacamento/3+espacamento/4+2);
		  	glVertex2i(LARGURA/2, ALTURA-espacamento/3+espacamento/4+2);
		glEnd();

		glColor3f(COR_VEZ_TEXTO); //seta cor.
		renderBitmapString(LARGURA/2-espacamento/2+7, ALTURA-espacamento/3-16+espacamento/4+2, GLUT_BITMAP_TIMES_ROMAN_10, "VEZ");


	}
	else{

		//seta para a esquerda:
		glColor3f(COR_VEZ_FUNDO); //seta cor.
		glBegin(GL_POLYGON);
		  	glVertex2i(LARGURA/2, ALTURA-espacamento/3+espacamento/4+2);
		  	glVertex2i(LARGURA/2, ALTURA-2*espacamento/3+espacamento/4+2);
		  	glVertex2i(LARGURA/2+espacamento/2, ALTURA-2*espacamento/3+espacamento/4+2);
		  	glVertex2i(LARGURA/2+espacamento/2, ALTURA-espacamento/3+espacamento/4+2);

		glEnd();

		//borda da seta para a esquerda:
		glColor3f(COR_LINHAS); //seta cor.
		glLineWidth(2);
		glBegin(GL_LINE_STRIP);
		  	glVertex2i(LARGURA/2, ALTURA-espacamento/3+espacamento/4+2);
		  	glVertex2i(LARGURA/2, ALTURA-2*espacamento/3+espacamento/4+2);
		  	glVertex2i(LARGURA/2+espacamento/2, ALTURA-2*espacamento/3+espacamento/4+2);
		  	glVertex2i(LARGURA/2+espacamento/2, ALTURA-espacamento/3+espacamento/4+2);
		  	glVertex2i(LARGURA/2, ALTURA-espacamento/3+espacamento/4+2);
		glEnd();

		glColor3f(COR_VEZ_TEXTO); //seta cor.
		renderBitmapString(LARGURA/2+espacamento/2-27, ALTURA-espacamento/3-16+espacamento/4+2, GLUT_BITMAP_TIMES_ROMAN_10, "VEZ");


	}

	//linha divisória:
	glColor3f(COR_LINHAS); //seta cor.
	glLineWidth(4);
	glBegin(GL_LINES);
	  	glVertex2i(LARGURA/2, ALTURA);
	  	glVertex2i(LARGURA/2, ALTURA-espacamento);
	glEnd();

}


//renderiza texto:
void renderBitmapString(float x, float y, void *font, char *string){  
	char *c;
	glRasterPos2f(x,y);
	for (c=string; *c != '\0'; c++){
		glutBitmapCharacter(font, *c);
	}
}

//desenha círculo:
void drawCircle(float x_centro, float y_centro, float raio, int cor){

	float x, y, x_center=x_centro, y_center=y_centro;
	float radius = raio;
	int j;
	glBegin(GL_POLYGON);
		if(cor == BRANCO)
			glColor3f(0.9f,0.9f,0.9f);
		else
			glColor3f(0.15f,0.15f,0.15f);

		x = (float)radius * cos(359 * acos(-1.0)/180.0f) + x_center;
		y = (float)radius * sin(359 * acos(-1.0)/180.0f) + y_center;
		for(j=0; j<360; j++){
			glVertex2f(x,y);
			x = (float)radius * cos(j * acos(-1.0)/180.0f) + x_center;
			y = (float)radius * sin(j * acos(-1.0)/180.0f) + y_center;
			glVertex2f(x,y);
		}
	glEnd();

	glLineWidth(1.11);
	glBegin(GL_LINE_LOOP);

		glColor3f(0.0f,0.0f,0.0f);

		x = (float)radius * cos(359 * acos(-1.0)/180.0f) + x_center;
		y = (float)radius * sin(359 * acos(-1.0)/180.0f) + y_center;
		for(j=0; j<360; j++){
			glVertex2f(x,y);
			x = (float)radius * cos(j * acos(-1.0)/180.0f) + x_center;
			y = (float)radius * sin(j * acos(-1.0)/180.0f) + y_center;
			glVertex2f(x,y);
		}
	glEnd();

	glLineWidth(1.11);
	glBegin(GL_LINE_LOOP);

		glColor3f(0.0f,0.0f,0.0f);
		radius -= 5;

		x = (float)radius * cos(359 * acos(-1.0)/180.0f) + x_center;
		y = (float)radius * sin(359 * acos(-1.0)/180.0f) + y_center;
		for(j=0; j<360; j++){
			glVertex2f(x,y);
			x = (float)radius * cos(j * acos(-1.0)/180.0f) + x_center;
			y = (float)radius * sin(j * acos(-1.0)/180.0f) + y_center;
			glVertex2f(x,y);
		}
	glEnd();

}


//funções de mapeamento:

float mapeiaCoordXEmTela(int a){
	return ((float)(a*espacamento + espacamento/2));
}

int mapeiaTelaXEmCoord(float a){
	return ((int)(a/espacamento));
}

float mapeiaCoordYEmTela(int a){
	return ((float)(ALTURA-(espacamento + a*espacamento + espacamento/2)));
}

int mapeiaTelaYEmCoord(float a){
	return ((int)((a-espacamento)/espacamento));
}

//função display:
void display(void){

	glClear(GL_COLOR_BUFFER_BIT); //limpa janela.

	switch(codTela){

		case NUM_JOGADORES:

			//desenhaTabuleiro
			desenhaTabuleiro();

			//escolher número de jogadores:
			desenhaEscolhaNumJogadores();

			break;

		case COR:

			//desenhaTabuleiro
			desenhaTabuleiro();

			//escolher cor da peça, caso seja 1 jogador.
			desenhaEscolhaCor();

			break;

		case FINAL:

			//desenhaTabuleiro
			desenhaTabuleiro();

			//desenhaPlacar
			desenhaPlacar();

			//desenhaPecas
			seta_tabuleiro ();
			desenhaPecas();

			//mostraVencedor
			desenhaVencedor();

			break;

		case JOGO:

			desenhaTabuleiro();

			desenhaPlacar();

			seta_tabuleiro ();
			desenhaPecas();

			desenhaVez();

			if (numJogadores == 1 && vez != corJogador && qtdBrancas == 2 && qtdPretas == 2)
				gui_refresh ();
		
			break;

	}

	//acaba:
	glutSwapBuffers();	
	glFlush();

	
}


//trata teclado:
void keyboard(unsigned char key, int x, int y){

	if(key == 'q'){

		//apertou 'q'...
		exit(0);

	}

}

//trata clique:
void mouse(int button, int state, int x, int y){

	
	if(button==GLUT_LEFT_BUTTON && state==GLUT_DOWN && podeClicar){

		switch(codTela){

			case NUM_JOGADORES:


				//se clicou no botao 1 jogador
				if(((x>=LARGURA/2-espacamento)&&(x<=LARGURA/2+espacamento)) && ((ALTURA-y>=ALTURA/2+espacamento/2-espacamento/4)&&(ALTURA-y<=ALTURA/2+3*espacamento/2-espacamento/4))){

					numJogadores = 1;
					codTela = COR;
					podeClicar = TRUE;
					glutPostRedisplay();

				}
				//se clicou no botao 2 jogadores
				else if(((LARGURA-x>=LARGURA/2-espacamento)&&(LARGURA-x<=LARGURA/2+espacamento)) && ((ALTURA-y>=ALTURA/2-3*espacamento/2+espacamento/4)&&(ALTURA-y<=ALTURA/2-espacamento/2+espacamento/4))){

					numJogadores = 2;
					codTela = JOGO;
					podeClicar = TRUE;
					glutPostRedisplay();

				}


				break;

			case COR:


				//se clicou no botao Preto
				if(((x>=LARGURA/2-espacamento)&&(x<=LARGURA/2+espacamento)) && ((ALTURA-y>=ALTURA/2+espacamento/2-espacamento/4)&&(ALTURA-y<=ALTURA/2+3*espacamento/2-espacamento/4))){

					corJogador = PRETO;
					codTela = JOGO;
					podeClicar = TRUE;
					glutPostRedisplay();

				}
				//se clicou no botao Branco
				else if(((x>=LARGURA/2-espacamento)&&(x<=LARGURA/2+espacamento)) && ((ALTURA-y>=ALTURA/2-3*espacamento/2+espacamento/4)&&(ALTURA-y<=ALTURA/2-espacamento/2+espacamento/4))){

					corJogador = BRANCO;
					codTela = JOGO;
					podeClicar = FALSE;
					glutPostRedisplay();

				}


				break;


			case JOGO:

				if(y < espacamento){
					return;
				}

				coordenadaX_aux = mapeiaTelaXEmCoord(x);
				coordenadaY_aux = mapeiaTelaYEmCoord(y);

				if(numJogadores==2 || vez==corJogador){

					if(tabuleiro[coordenadaX_aux][coordenadaY_aux] == POSSIVEL_JOGADA){ //se possível a inserção...

						coordenadaX = coordenadaX_aux;
						coordenadaY = coordenadaY_aux;
						
						gui_refresh ();
						
						while (numJogadores == 1 && vez != corJogador && codTela != FINAL)
							gui_refresh ();

					}
				}

				break;


		}


	}

}


