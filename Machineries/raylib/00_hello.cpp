#include <raylib.h>

int main(int, char**) {
  InitWindow(500, 500, "hello!");
  SetTargetFPS(60);

  Rectangle rec = {0, 50, 200, 200};

  while (!WindowShouldClose()) {
    BeginDrawing();
    DrawFPS(0, 0);
    ClearBackground(RAYWHITE);

    Color c = CheckCollisionPointRec(GetMousePosition(), rec) ? RED : YELLOW;

    DrawRectangleRec(rec, c);

    rec.x += 1;
    if (rec.x >= 500 - 200) {
      rec.x = 0;
    }

    DrawText("Hello,World", 100, 100, 50, SKYBLUE);
    EndDrawing();
  }

  CloseWindow();
  return 0;
}
