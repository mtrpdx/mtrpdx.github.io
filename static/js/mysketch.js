let i = 0;
let j = 0;

function setup() {
  // put setup code here
    smooth();
    var canvas = createCanvas(240, 240);
    setAttributes('antialias', true);

    canvas.parent('p5-sketch');

    frameRate(30);
}

function draw() {

  // put drawing code here
    strokeWeight(4);
    stroke(153);
    let a = 0.0;
    let inc = TWO_PI / 48.0;
    for (let i = 0; i < 50; i=i+2) {
      line(i * 4.8, 110, i *4.8, 110 + sin(a) * 80.0);
      a = a + inc;
    }

}
