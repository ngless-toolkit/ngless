var margin = {top: 20, right: 80, bottom: 30, left: 50},
    width = 960 - margin.left - margin.right,
    height = 500 - margin.top - margin.bottom;


var x = d3.scale.linear()
    .range([0, width]);

var y = d3.scale.linear()
    .range([height, 0]);

var color = d3.scale.category10();

var xAxis = d3.svg.axis()
    .scale(x)
    .orient("bottom")
    .ticks(data.length)
    .tickSize(-height, 0, 0);

var yAxis = d3.svg.axis()
    .scale(y)
    .orient("left");

var line = d3.svg.line()
    .interpolate("basis")
    .x(function(d) { return x(d.bp); })
    .y(function(d) { return y(d.dataSet); });

var svg = d3.select("body").append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");


color.domain(d3.keys(data[0]).filter(function(key) { return key !== "bp"; }));

var statistics = color.domain().map(function(name) {
return {
  name: name,
  values: data.map(function(d) {
	return {bp: d.bp, dataSet: +d[name]};
  })
};
});

x.domain(d3.extent(data, function(d) { return d.bp; }));

y.domain([
d3.min(statistics, function(c) { return d3.min(c.values, function(v) { return v.dataSet; }); }),
d3.max(statistics, function(c) { return d3.max(c.values, function(v) { return v.dataSet; }); })
]);

svg.append("g")
  .attr("class", "x axis")
  .attr("transform", "translate(0," + height + ")")
  .call(xAxis)
.append("text")
  .attr("x", width / 2 )
  .attr("y",  margin.bottom)
  .style("text-anchor", "middle")
  .text("Quality Scores across all base pairs");

svg.append("g")
  .attr("class", "y axis")
  .call(yAxis);

var statistic = svg.selectAll(".statistic")
  .data(statistics)
  .enter()
  .append("g")
  .attr("class", "line");

statistic.append("path")
  .attr("class", "line")
  .attr("d", function(d) { return line(d.values); })
  .style("stroke", function(d) { return color(d.name); });

statistic.append("text")
  .datum(function(d) { return {name: d.name, value: d.values[d.values.length - 1]}; })
  .attr("transform", function(d) { return "translate(" + x(d.value.bp) + "," + y(d.value.dataSet) + ")"; })
  .attr("x", 3)
  .attr("dy", ".35em")
  .text(function(d) { return d.name; });