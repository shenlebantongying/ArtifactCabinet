#include <raylib.h>
#include <stdio.h>
constexpr float u = 200.0f;
constexpr float u3 = u * 3;

void DrawGrid() {
  int t = 6;
  for (int i = 0; i < t; ++i) {
    DrawLineV(Vector2{u3 / t * i, 0}, Vector2{u3 / t * i, u3}, BLACK);
    DrawLineV(Vector2{0, u3 / t * i}, Vector2{u3, u3 / t * i}, BLACK);
  }
}

int main(int, char **) {
  InitWindow(u3, u3, "hello!");
  SetTargetFPS(60);

  Rectangle rec = {-u / 2.0, -u / 2.0, u, u};

  Camera2D cam = {.offset = {u3 / 2, u3 / 2},
                  .target = {0, 0},
                  .rotation = 0,
                  .zoom = 1.0f};

  Color cubeColor = VIOLET;
  cubeColor.a = 0xCC;
  const float step = u / 4;
  while (!WindowShouldClose()) {
    int key = GetKeyPressed();
    switch (key) {
      case KEY_W: {
        cam.offset.y -= step;
        break;
      }
      case KEY_S: {
        cam.offset.y += step;
        break;
      }
      case KEY_A: {
        cam.offset.x -= step;
        break;
      }
      case KEY_D: {
        cam.offset.x += step;
        break;
      }
      case KEY_Q: {
        cam.rotation -= 15;
        break;
      }
      case KEY_E: {
        cam.rotation += 15;
        break;
      }
      case KEY_R: {
        cam.zoom += 0.5;
        break;
      }
      case KEY_F: {
        cam.zoom -= 0.5;
        break;
      }
      default: {
        if (key != KEY_NULL) {
          printf("%d\n", key);
        }
        break;
      }
    }

    BeginDrawing();
    ClearBackground(RAYWHITE);

    DrawGrid();

    BeginMode2D(cam);
    {
      DrawRectangleRec(rec, cubeColor);

      EndMode2D();
    }

    EndDrawing();
  }

  CloseWindow();
  return 0;
}
