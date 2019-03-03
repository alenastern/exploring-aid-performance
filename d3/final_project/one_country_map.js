// create svg and g blocks
//import {geoPath, geoAlbersUsa} from 'd3-geo';
//import {select} from 'd3-selection';
// https://stackoverflow.com/questions/27319304/how-to-have-one-event-call-multiple-functions-d3-javascript


document.addEventListener('DOMContentLoaded', () => {
    Promise.all(['./ghana_2.geojson',
    './d3_ghana_data.json'
    ].map(url => fetch(url).then(data => data.json())))
    .then(data => myVis(data)).catch(error => {
          console.log(`The following error has occured: ${error}`)
      })
  });


function myVis(data) {

    const [ghanaShapes, ghanaWB] = data;
    const width = 800
    const height = 900
    const margin = {
        top: 10,
        left: 10,
        right: 10,
        bottom: 10
    };

    
    const projection = d3.geoEquirectangular()
        .center([0, 7.0])
        .rotate([1.2, 0])
        .scale(6000)
        .translate([width/2,height/2]);
        
    const path = d3.geoPath()
        .projection(projection);
    
    const svg =  d3.select('.first')
        .append('svg')
        .attr('width', width + margin.left + margin.right)
        .attr('height', height + margin.top + margin.bottom)

    const colorRange = ['#125169', '#2699D0', '#a9d9ef','#fabe7a', '#F89E37', '#b83d05'];
    const color = d3.scaleOrdinal()
      .domain([6, 5, 4, 3, 2, 1])
      .range(colorRange);

    //tooltip + mouseover
    const tooltip = d3.select(".first").append("div")
                  .attr("class", "tooltip")
                  .style("opacity", 0);

    const tipMouseover = function(d) {
      const html  = "<b>Title:</b> " + d.project_title + "<br/>" +
                    "<b>Start Date:</b> " + d.start_actual_isodate + " <b>End Date:</b> " + d.end_actual_isodate + "<br/>" + 
                  "<b>Funding:</b> " +  d.total_commitments + "<br/>" +
                  "<b>Performance:</b> " + d.performance_cat + "<br/>" +
                  "<b>Goal:</b> " + d.goal;

      tooltip.html(html)
          .style("left", (d3.event.pageX + 15) + "px")
          .style("top", (d3.event.pageY - 28) + "px")
          .transition()
            .duration(200) 
            .style("opacity", .9) 
    };
  // tooltip mouseout event handler
  var tipMouseout = function(d) {
      tooltip.transition()
          .duration(300) 
          .style("opacity", 0);
  };

    svg.selectAll('path')
      .data(ghanaShapes.features)
      .enter()
      .append('path')
      .attr('d', path)
      .attr('fill', '#e9e9e9') 
      .attr('stroke', 'black');

    const circles=  svg.selectAll('circle')
        .data(ghanaWB)
        .enter()
        .append('circle')
        //.attr("class", function(d) { return d.project_id; })
        .attr("cx", function(d) {
            return projection([d.longitude, d.latitude])[0];
        })
        .attr("cy", function(d) {
            return projection([d.longitude, d.latitude])[1];
        })
        .attr('fill', d => color(d.six_overall_rating))
        .attr('stroke-width', 0.25)
        .attr('opacity', 0.60)

        circles.transition()
                    .duration(3000)
                    .attr("r", function(d) {
                        return Math.sqrt(parseInt(d.even_split_commitments) * 0.000008);
                 });
        circles.on("mouseover", tipMouseover);
        circles.on("mouseout", tipMouseout);

        svg.append("text")             
        .attr("transform",
        "translate(" + (width/2 + margin.left) + " ," + 
                       (height + margin.top + margin.bottom - 20) + ")")
            .style("text-anchor", "left")
            .text("Source: Project Performance Database (Honig 2018)")
            .style("font-family", '"Lucida Console", monospace')
            .style("font-size", "10px");

  
    
        
}  


