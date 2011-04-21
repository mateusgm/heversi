#ifndef GUI_H
#define GUI_H

#include <math.h>
#include <string.h>
#include <GL/glut.h>

#include "GUI/SOIL.h"
#include "jogo.h"

// definições interface gráfica

#define FOREVER 1

#define TRUE		1
#define FALSE		0

#define MAX_NUM		10

#define LARGURA		560
#define ALTURA		630

#define NUM_JOGADORES	0
#define COR		1
#define JOGO		2

#define FINAL		3

#define RAIO		25.0f

#define POSICAO_JANELA		200, 55

//cores:

#define COR_TABULEIRO_FUNDO	0.6f, 0.8f, 1.0f, 0.0f
#define COR_PLACAR		0.38671875f, 0.00390625f, 0.0078125f
#define COR_LINHAS		0.5f, 0.1f, 0.1f
#define COR_VEZ_FUNDO		0.90234375f, 0.70703125f, 0.3671875f
#define COR_POPUP_TEXTO		0.38671875f, 0.00390625f, 0.0078125f
#define COR_TITULO		0.38671875f, 0.00390625f, 0.0078125f
#define COR_BOTAO_TEXTO		0.38671875f, 0.00390625f, 0.0078125f
#define COR_VEZ_TEXTO		0.38671875f, 0.00390625f, 0.0078125f

// variaveis

extern int podeClicar;
extern int espacamento;
extern int numJogadores;
extern int corJogador;
extern int vez;
extern int coordenadaX;
extern int coordenadaY;
extern int codTela;
extern int coordenadaX_aux;
extern int coordenadaY_aux;

extern GLuint textura_tabuleiro;
extern GLuint textura_placar;
extern GLuint textura_popup;
extern GLuint textura_botao;

//funções interface gráfica

void gui_init (int argc, char** argv);
void gui_start (void);
void gui_refresh (void);

void desenhaPlacar(void);
void desenhaPecas(void);
void desenhaEscolhaNumJogadores(void);
void desenhaEscolhaCor(void);
void desenhaVez(void);

void renderBitmapString(float x, float y, void *font, char *string);
void drawCircle(float x_centro, float y_centro, float raio, int cor);

float mapeiaCoordXEmTela(int a);
float mapeiaCoordYEmTela(int a);
int mapeiaTelaXEmCoord(float a);
int mapeiaTelaYEmCoord(float a);

void display(void);
void keyboard(unsigned char key, int x, int y);
void mouse(int button, int state, int x, int y);

#endif
