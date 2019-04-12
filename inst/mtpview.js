// INITIALIZE SECTION

// {well:A01} -> {name: A01, row: A, col: 1}
let parseWell = function(input) {
  let re = new RegExp('([A-Z]+)0?([\\d]+)');
  let result = re.exec(input);
  return ({name: result[0], row: result[1], col: +result[2]});
};

data.map(function(d) {
  d.pos = parseWell(d.well);
  return(d);
});

// add plate skeleton
function mtpAdd1(selection,dat) {

  let mtp = selection.append('g')
    .append('svg')
    .attr('class', 'plate')
    .attr('viewBox', '0 0 127.76 85.48')
    .attr('preserveAspectRatio', 'xMidYMid meet');
  // footprint rect
  mtp.append('g')
    .attr('class', 'footprint')
    .append('rect')
    .attr('fill','none')
    .attr('stroke-width', 0.25)
    .attr('stroke','#ccc')
    .attr('width',127.76)
    .attr('height', 85.48)
    .attr('rx', 4)
    .attr('ry', 4);
  // outer-border
   mtp.append('g')
    .attr('class', 'outer-border')
    .append('polyline')
    .attr('stroke', '#bbb')
    .attr('fill', 'none')
    .attr('stroke-width', 0.25)
    .attr('points', '7,2 125.76,2 125.76,83.48 7,83.48 2,78.48 2,7 7,2');

  // scales to position wells
  let Cols = d3.set(dat.map(d => d.pos.col)).values();
  let nCols = Cols.length;
  let x = d3.scaleBand()
    .domain(Cols)
    .range([0 + 10.9, 127.76 - 10.9])
    .paddingInner(0.227);

  let Rows = d3.set(dat.map(d => d.pos.row)).values();
  let nRows = Rows.length;
  let y = d3.scaleBand()
    .domain(Rows)
    .range([0 + 7.76,85.48 - 7.76])
    .paddingInner(0.227)
    .paddingOuter(0);

  // how much rounding depends on nWells
  let wellR;
  switch (nRows) {
    case 4:
        wellR = 10;
        break;
    case 8:
        wellR = 5;
        break;
    case 16:
        wellR = 1;
}

   // inner-border
   mtp.append('rect')
    .attr('class', 'inner-border')
    .attr('fill','none')
    .attr('stroke-width', 0.1)
    .attr('stroke','#ccc')
    .attr('x', x(1) - 1)
    .attr('y', y('A') - 1)
    .attr('width', x.step() * (nCols - 1) + x.bandwidth() + 2)
    .attr('height', y.step() * (nRows - 1) + y.bandwidth() + 2)
    .attr('rx', 1)
    .attr('ry', 1);
  // plate column labels
  mtp.append('g')
     .attr('class', 'plate-x-axis')
     .attr('transform', `translate(0,${y.step() * 0.75 + 7.76})`)
     .call(d3.axisTop(x))
     .call(g => g.select(".domain").remove())
     .call(g => g.selectAll(".tick line").remove())
     .call(g => g.selectAll(".tick text")
     .attr("font-family", "Monaco")
     .attr("font-size", 3)
     .attr("font-weight", "bold")
     .attr("fill", "grey")
     .attr("text-anchor", "middle")
     .attr("dominant-baseline", "baseline"));
    // plate row labels
    mtp.append('g')
      .attr('class', 'plate-y-axis')
      .attr('transform', `translate(${x.step() * 0.75 + 10},0)`)
      .call(d3.axisLeft(y))
      .call(g => g.select(".domain").remove())
      .call(g => g.selectAll(".tick line").remove())
      .call(g => g.selectAll(".tick text")
      .attr("font-family", "Monaco")
      .attr("font-size", 3)
      .attr("font-weight", "bold")
      .attr("fill", "grey")
      .attr("text-anchor", "end")
      .attr("dominant-baseline", "center"));

  mtp.selectAll('g.well')
    .data(dat)
    .enter().append('g')
    .attr('class', 'well')
    .each(function(d) {
    d3.select(this)
    .attr('transform', `translate(${x(d.pos.col)},${y(d.pos.row)})`)
    .append('rect')
    .attr('stroke-width', 0.25)
    .attr('stroke','#ccc')
    .attr('fill', "none")
    .attr('width', x.bandwidth())
    .attr('height', y.bandwidth())
    .attr('rx', wellR)
    .attr('ry', wellR)
    .attr('well', d.well.label);
  });
}


// RENDER SECTION

// add well data
function mtpAdd2(selection, dat, fillVar, fillOpacityVar) {

  var fill = d3.scaleOrdinal(d3.schemeCategory10)
    .domain(d3.set(dat.map(d => d[fillVar])).values());

  let fillOpacity = d3.scaleLinear()
    .domain(d3.extent(dat, d => d.conc))
    .range([0,1]);

  let tooltip = d3.select("body")
    .append("div")
    .style("position", "absolute")
    .style("z-index", "10")
    .style("visibility", "hidden")
    .style("color", "white")
    .style("padding", "8px")
    .style("background-color", "rgba(0, 0, 0, 0.75)")
    .style("border-radius", "6px")
    .style("font", "12px sans-serif")
    .text("tooltip")
    .style("visibility", "hidden");

  selection.selectAll('svg g.well rect')
    .attr('fill', d => fill(d[fillVar]))
    .attr('fill-opacity', d => fillOpacity(d[fillOpacityVar]))
    .on("mouseover", function(d) {
      let ttText = d.pos.name + " / ";
          ttText += fillVar + ": " + d[fillVar] + " / ";
          ttText += fillOpacityVar + ": " + d[fillOpacityVar];

              tooltip.text(ttText);
              tooltip.style("visibility", "visible");
      })
    .on("mousemove", () => tooltip.style("top", (d3.event.pageY-10)+"px").style("left",(d3.event.pageX+10)+"px"))
    .on("mouseout", () => tooltip.style("visibility", "hidden"));


}


// make the plot
svg.call(mtpAdd1, data);
svg.call(mtpAdd1, data);

if (options.fillVar !== null) {
    svg.call(mtpAdd2, data, options.fillVar, options.fillOpacityVar);
}
