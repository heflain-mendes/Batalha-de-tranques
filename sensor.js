class Sensor{
    constructor(car){
        this.car=car;
        this.rayCount=5;
        this.rayLength=250;
        this.raySpread=Math.PI/2;

        this.rays=[];
        this.readings=[];
    }

    update(arenaBorders,tanks){
        this.#castRays();
        this.readings=[];
        for(let i=0;i<this.rays.length;i++){
            this.readings.push(
                this.#getReading(
                    this.rays[i],
                    arenaBorders,
                    tanks
                )
            );
        }
    }

    #getReading(ray,arenaBorders,tanks){
        let touches=[];

        for(let i=0;i<arenaBorders.length;i++){
            const touch=getIntersection(
                ray[0],
                ray[1],
                arenaBorders[i][0],
                arenaBorders[i][1]
            );
            if(touch){
                touches.push(touch);
            }
        }

        for(let i=0;i<tanks.length;i++){
            const poly=tanks[i].polygon;
            for(let j=0;j<poly.length;j++){
                const value=getIntersection(
                    ray[0],
                    ray[1],
                    poly[j],
                    poly[(j+1)%poly.length]
                );
                if(value){
                    //value.score = tanks[i].score;
                    touches.push(value);
                }
            }
        }
        if(touches.length==0){
            return null;
        }else{
            const offsets=touches.map(e=>e.offset);
            const minOffset=Math.min(...offsets);
            return touches.find(e=>e.offset==minOffset);
        }
        
    }

    #castRays(){
        this.rays=[];
        const start={x:this.car.x, y:this.car.y};
        var rayAngle, end;
        for(let i=0;i<this.rayCount;i++){
            rayAngle=lerp(
                this.raySpread/2,
                -this.raySpread/2,
                this.rayCount==1?0.5:i/(this.rayCount-1)
            )+this.car.angle;
    
            end={
                x:this.car.x-
                    Math.sin(rayAngle)*this.rayLength,
                y:this.car.y-
                    Math.cos(rayAngle)*this.rayLength
            };
            this.rays.push([start,end]);
        }
        rayAngle=lerp(
            this.raySpread/2,
            -this.raySpread/2,
            1.5
        )-this.car.angle;
        end={
            x:this.car.x-Math.cos(rayAngle)*this.rayLength,
            y:this.car.y-Math.sin(rayAngle)*this.rayLength,
        };
        this.rays.push([start,end]);
    }

    draw(ctx){
        for(let i=0;i<this.rays.length;i++){
            let end=this.rays[i][1];
            if(this.readings[i]){
                end=this.readings[i];
            }

            ctx.beginPath();
            ctx.lineWidth=2;
            ctx.strokeStyle="yellow";
            ctx.moveTo(
                this.rays[i][0].x,
                this.rays[i][0].y
            );
            ctx.lineTo(
                end.x,
                end.y
            );
            ctx.stroke();

            ctx.beginPath();
            ctx.lineWidth=2;
            ctx.strokeStyle="black";
            ctx.moveTo(
                this.rays[i][1].x,
                this.rays[i][1].y
            );
            ctx.lineTo(
                end.x,
                end.y
            );
            ctx.stroke();
        }
    }        
}