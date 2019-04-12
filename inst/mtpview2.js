// !preview r2d3 data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20), options = list(nRows = 8, nCols = 12, wellShape = "rect", borderStroke = "#bbb", borderStrokeWidth = 0.1)
//
// r2d3: https://rstudio.github.io/r2d3
//

class Microplate {
    constructor(nRows, nCols, wellShape) {
        this.rows = this.rowDomain(nRows);
        this.cols = this.colDomain(nCols);
        this.width = 127.76;
        this.height = 85.48;
        this.wellShape = wellShape;
        this.hWellPadding = 10.9;
        this.vWellPadding = 7.76;
        this.iWellPadding = 0.227; // percent
    }

  wells() {
    return d3.cross(this.rows, this.cols, (row, col) => ({row: row, col: col}));
  }

  rowDomain(n) {
    const alphabet = 'abcdefghijklmnopqrstuvwxyz'.toUpperCase().split('');
    return alphabet.slice(0, n);
  }

  colDomain(n) {
    return Array.apply(null, {length: n}).map(Number.call, Number).map(d => d+1);
  }

  xScale() {
    return d3.scaleBand()
                .domain(this.cols)
                .range([0 + this.hWellPadding, this.width - this.hWellPadding])
                .paddingInner(this.iWellPadding);
  }

  yScale() {
    return d3.scaleBand()
                .domain(this.rows)
                .range([0 + this.vWellPadding, this.height - this.vWellPadding])
                .paddingInner(this.iWellPadding)
                .paddingOuter(0);
  }

}

function addMicroplate(d,i) {

  const sel = d3.select(this);
  const microplate = new Microplate(options.nRows, options.nCols, options.wellShape);
  const x = microplate.xScale();
  const y = microplate.yScale();

  const plateSVG = sel.append('svg')
    .attr('class', 'plate')
    .attr('viewBox', [0, 0, microplate.width, microplate.height].join(' '))
    .attr('preserveAspectRatio', 'xMidYMid meet')
    .attr('width', width)
    .attr('height', height);

  plateSVG.append('rect')
    .attr('class', 'footprint')
    .attr('fill','none')
    .attr('stroke-width', options.borderStrokeWidth)
    .attr('stroke', options.borderStroke)
    .attr('width', microplate.width)
    .attr('height', microplate.height)
    .attr('rx', 4)
    .attr('ry', 4);

  plateSVG.append('polyline')
    .attr('class', 'outer-border')
    .attr('stroke', options.borderStroke)
    .attr('fill', 'none')
    .attr('stroke-width', options.borderStrokeWidth)
    .attr('points', '7,2 125.76,2 125.76,83.48 7,83.48 2,78.48 2,7 7,2');

  plateSVG.append('rect')
    .attr('class', 'inner-border')
    .attr('fill','none')
    .attr('stroke-width', options.borderStrokeWidth)
    .attr('stroke', options.borderStroke)
    .attr('x', x(1) - 1)
    .attr('y', y('A') - 1)
    .attr('width', x.step() * (microplate.cols.length - 1) + x.bandwidth() + 2)
    .attr('height', y.step() * (microplate.rows.length - 1) + y.bandwidth() + 2)
    .attr('rx', 1)
    .attr('ry', 1);

  plateSVG.append('g')
    .attr('class', 'plate-x-axis')
    .attr('transform', `translate(0,${microplate.vWellPadding / 2})`)
    .call(d3.axisTop(x))
    .call(g => g.select(".domain").remove())
    .call(g => g.selectAll(".tick line").remove())
    .call(g => g.selectAll(".tick text")
                  .attr("font-family", "Monaco")
                  .attr("font-size", 3)
                  .attr("font-weight", "bold")
                  .attr("fill", "grey")
                  .attr("text-anchor", "middle")
                  .attr('y', 1)
                  .attr("alignment-baseline", "middle"));

  plateSVG.append('g')
    .attr('class', 'plate-y-axis')
    .attr('transform', `translate(${microplate.hWellPadding / 2},0)`)
    .call(d3.axisLeft(y))
    .call(g => g.select(".domain").remove())
    .call(g => g.selectAll(".tick line").remove())
    .call(g => g.selectAll(".tick text")
                  .attr("font-family", "Monaco")
                  .attr("font-size", 3)
                  .attr("font-weight", "bold")
                  .attr("fill", "grey")
                  .attr("text-anchor", "middle")
                  .attr('x', 1)
                  .attr("alignment-baseline", "middle"));

  const wells = plateSVG.selectAll('g.well')
      .data(microplate.wells())
      .enter().append('g')
    .attr('class', 'well')
    .attr('transform', d => `translate(${x(d.col)},${y(d.row)})`);

  wells.each(function(d) {
    if (microplate.wellShape === 'rect') {
      d3.select(this).append('rect')
       .attr('class', 'well-border')
       .attr('stroke-width', options.borderStrokeWidth)
       .attr('stroke',options.borderStroke)
       .attr('fill', 'none')
       .attr('width', x.bandwidth())
       .attr('height', y.bandwidth())
       .attr('rx', 0)
       .attr('ry', 0);
    }
    if (microplate.wellShape === 'circle') {
      d3.select(this).append('circle')
       .attr('class', 'well-border')
       .attr('stroke-width', 0.25)
       .attr('stroke','#bbb')
       .attr('fill', 'none')
       .attr('r', x.bandwidth() / 2)
       .attr('cx', x.bandwidth() / 2)
       .attr('cy', y.bandwidth() / 2);
    }
  });

}


svg.each(addMicroplate);

r2d3.onRender(function(data, svg, width, height, options) {
  svg.selectAll('.well-border').each(function(d, i) {
    var odd = i % 2 === 1;
    d3.select(this).style('fill', odd ? 'none' : '#ddd');
  });


});

