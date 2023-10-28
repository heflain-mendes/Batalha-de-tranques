class Arena {
    constructor(height,width,padding){
        this.padding=padding;
        this.height=height;
        this.width=width;

        this.left=padding;
        this.right=width-padding;
        this.top=padding;
        this.bottom=height-padding;

        const topLeft={x:this.left,y:this.top};
        const topRight={x:this.right,y:this.top};
        const bottomLeft={x:this.left,y:this.bottom};
        const bottomRight={x:this.right,y:this.bottom};
        this.borders=[
            [topLeft,bottomLeft],
            [topRight,bottomRight],
            [topLeft,topRight],
            [bottomLeft,bottomRight]
        ];
        this.position = {left:this.left, right:this.right, top: this.top, bottom:this.bottom};
    }

    draw(ctx){
        ctx.clearRect(0,0,this.width,this.height);
        ctx.lineWidth=5;
        ctx.strokeStyle="white";
        this.borders.forEach(border=>{
            ctx.beginPath();
            ctx.moveTo(border[0].x,border[0].y);
            ctx.lineTo(border[1].x,border[1].y);
            ctx.stroke();
        });
    }
}