const c = document.querySelector("#c");

function drawEdges(ctx, W, H) {
  ctx.moveTo(1, 1);
  ctx.lineTo(W - 1, 1);
  ctx.lineTo(W - 1, H - 1);
  ctx.lineTo(1, H - 1);
  ctx.closePath();
  ctx.stroke();
}

function drawPoly(ctx, n_edges, radius, rotate) {
  ctx.save();

  ctx.rotate(rotate);

  for (let i = 0; i <= n_edges; i++) {
    const rotation = (Math.PI / (n_edges / 2)) * i;

    if (i === 0) {
      ctx.moveTo(radius * Math.cos(rotation), radius * Math.sin(rotation));
    } else {
      ctx.lineTo(radius * Math.cos(rotation), radius * Math.sin(rotation));
    }
  }

  ctx.restore();
}

const W = 500;
const H = 500;

c.width = W;
c.height = H;

const ctx = c.getContext("2d");
drawEdges(ctx, W, H);

ctx.translate(W / 2, H / 2);

drawPoly(ctx, 6, 200, 0);
drawPoly(ctx, 6, 200, Math.PI / 2);

ctx.closePath();
ctx.stroke();
