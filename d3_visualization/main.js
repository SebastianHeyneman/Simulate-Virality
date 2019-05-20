function randomIntFromInterval(min,max) // min and max included
{
    return Math.floor(Math.random()*(max-min+1)+min);
}

// DROPDOWN
var f = document.querySelector("#header"),
    h = document.querySelector("#dismiss"),
    ka = h.innerHTML;
    (f.classList.toggle("collapsed", !0),
    h.innerHTML = "DISCOVER", f.scrollTop = 0)

h.addEventListener("click", function() {
    h.innerHTML === ka ?
    (f.classList.toggle("collapsed", !0),
    h.innerHTML = "DISCOVER", f.scrollTop = 0) : (f.classList.toggle("collapsed", !1), h.innerHTML = ka)
})

// CUSTOMERS AND STAKEHOLDERS
const customers = {};
const stakeholders = {};
const cumulative_series = {};

// Set SVG
width_base = 900
height_base = 360

// Dimensions of chart.
const margin = { top: 20, right: 20, bottom: 20, left: 20 },
      width = width_base*2 - margin.left - margin.right,
      height = height_base*2 - margin.top - margin.bottom;

const svg = d3.select("#chart").append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
    .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

d3.select("#chart").style("width", (width+margin.left+margin.right)+"px");

// LOCATIONS
var circles_x = 300
var circles_y = 100

var rect_x = 0
var rect_y = 520

var table_width = 500
var table_height = 500
var table_x = 770
var table_y = 200

var width_r = 225
var height_r = 75

var line_x = 800
var line_y = 430
var line_width = 250
var line_height = 200

var extent_height = 500
var extent_width = 700

const quarters = ["2012 Q1", "2012 Q2", "2012 Q3", "2012 Q4",
                  "2013 Q1", "2013 Q2", "2013 Q3", "2013 Q4",
                  "2014 Q1", "2014 Q2", "2014 Q3", "2014 Q4",
                  "2015 Q1", "2015 Q2", "2015 Q3", "2015 Q4",
                  "2016 Q1", "2016 Q2"]

var backdrops = {
  "unaware": {x: 0, y: 15, fill: "#f4f6f7", label: "No investments use Carta"},
  "aware": {x: 0, y: 170, fill: "#F7F7FC", label: "One or more investments use Carta"},
  "partner": {x: 0, y: 325, fill: "#FFFDF8", label: "One or more investments use Carta and refers investments to Carta"}
}

// PARAMETERS
const radius = 1.5
let time_so_far = 0;
const tick_time = 3000

// FUNNEL BACKDROPS
var backdrop_rects = svg.selectAll(".backdrop_rect")
                      .data(d3.values(backdrops))
                      .enter()
                      .append("rect")

    backdrop_rects
      .attr("x", d => d.x)
      .attr("y", d => d.y)
      .attr("height", 150)
      .attr("width", 675)
      .attr("fill", d => d.fill)

var backdrop_labels = svg.selectAll(".backdroptext")
                         .data(d3.values(backdrops))
                         .enter()
                         .append("text")

              backdrop_labels
                .attr("x", d => d.x + 10)
                .attr("y", d => d.y + 20)
                .text(d => d.label)
                .attr("class", "subtitle")

// CUSTOMER COUNT
var journey = {
  "seed": { x: rect_x, y: rect_y, fullname: "Seed", cnt: 0, row: 1 },
  "a":{ x: rect_x + (width_r), y: rect_y, fullname: "Series A", cnt: 0, row: 1 },
  "b":{ x: rect_x + (width_r * 2), y: rect_y, fullname: "Series B", cnt: 0, row: 1},
  "c":{ x: rect_x, y: rect_y + (height_r), fullname: "Series C", cnt: 0, row: 2},
  "d":{ x: rect_x + (width_r), y: rect_y + (height_r), fullname: "Series D", cnt: 0, row: 2},
  "d+":{ x:rect_x + (width_r * 2), y: rect_y + (height_r), fullname: "Post Series D", cnt: 0, row: 2},
}

// STAKEHOLDER GROUPS
const groups = {
    "unaware": { x: 290, y: 78, color: "#0aa5c2", cnt: 0, fullname: "Unaware" },
    "aware": { x: 290, y: 258, color: "#BEE5AA", cnt: 0, fullname: "Aware" },
    "partner": { x: 290, y: 410, color: "#93D1BA", cnt: 0, fullname: "Partner" }
};

// SOURCE COUNT
var sources = {
  "marketing": {Customers: 0, all_time: 0, Channel: "Marketing"},
  "referral_partners": {Customers: 0, all_time: 0, Channel: "Partners"}
}

// SOURCE TABLE
var columns = ["Channel", "Customers"]

var table = d3.select("g")
              .append("foreignObject")
              .attr("x", table_x)
              .attr("y", table_y)
              .attr("width", table_width)
              .attr("height", table_height)
              .append("xhtml:table")

var table_header = table.append("thead")
                        .append("tr")
                        .selectAll("th")
                        .data(columns)
                        .enter()
                        .append("th")
                        .text(d => d)

var table_body = table.append("tbody")
                      .selectAll("tr")
                      .data(d3.values(sources))
                      .enter()
                      .append("tr")
                      .attr("class", "tbl_body")

var cells = table_body.selectAll("td")
                      .data(d => columns.map(i => ({column: i, value: d[i]})))
                      .enter()
                      .append("td")
                      .attr("class", "info_box")
                      .text(d => d.value)

    table_body.sort(function(a, b)
                        { return (b["all_time"]) - (a["all_time"])})

// PERFORMANCE LINE CHART
var lineSVG = svg.append("g")
                .attr("class", "line-chart")
                .attr("width", line_width)
                .attr("height", line_height)
                .attr("transform", "translate(" + line_x + "," + line_y + ")")

var line_color = d3.scaleOrdinal()
              .domain([0, 1])
              .range(["#FFC423", "#FF5823"])

// LOAD DATA
Promise.all([
    d3.tsv("stages.tsv", d3.autoType),
    d3.tsv("customers.tsv", d3.autoType),
    d3.tsv("cumulative.tsv", d3.autoType),
])

   // Once data is loaded...
   .then(function(files){
  // Prepare the data...

  const stage_data = files[0]
  const customer_data = files[1]
  const chart_data = files[2]

    // Consolidate stages by id.
    stage_data.forEach(d => {
        if (d3.keys(stakeholders).includes(d.id+"")) {
            stakeholders[d.id+""].push(d);
        } else {
            stakeholders[d.id+""] = [d];
        }
    });

    // Consolidate customers by week.
    customer_data.forEach(d => {
        if (d3.keys(customers).includes(d.quarter+"")) {
            customers[d.quarter+""].push(d);
        } else {
            customers[d.quarter+""] = [d];
        }
    });

    // Consolidate time series data by group
    chart_data.forEach(d => {
        if (d3.keys(cumulative_series).includes(d.investor_type_frmted)) {
            cumulative_series[d.investor_type_frmted]["values"].push(d.cumsum);
        } else {
           cumulative_series[d.investor_type_frmted] = {"name": [], "values": []}
           cumulative_series[d.investor_type_frmted]["name"] = d.investor_type_frmted
        }
    });

    var weeks = d3.keys(cumulative_series["Marketing"]["values"])

    var x = d3.scaleLinear()
       .domain([1, d3.max(chart_data, d => d.week)]).nice()
       .range([0, line_width])

    var y = d3.scaleLinear()
       .domain([0, d3.max(chart_data, d => d.cumsum)]).nice()
       .range([line_height, 0])

    var xAxis = lineSVG.append("g")
                  .attr("class", "xaxis")
                  .attr("transform", "translate(0," + (line_height) + ")")
                  .call(d3.axisBottom(x).ticks(5))
                  .append("text")
                    .attr("x", 220)
                    .attr("y", 25)
                    .attr("dy", "0.32em")
                    .style("text-anchor", "start")
                    .style("fill", "#000")
                    .style("font-weight", "bold")
                    .text("Weeks");

    var yAxis =  lineSVG.append("g")
                    .attr("class", "yaxis")
                    .attr("transform", "translate(0,0)")
                    .call(d3.axisLeft(y))
                    .append("text")
                      .attr("x", 4)
                      .attr("y", 0.5)
                      .attr("dy", "0.32em")
                      .style("text-anchor", "start")
                      .style("fill", "#000")
                      .style("font-weight", "bold")
                      .text("Customers");

    var line = d3.line()
       .defined(d => !isNaN(d))
       .x((d, i) => x(weeks[i]))
       .y(d => y(d))

    const path = lineSVG.append("g")
        .attr("fill", "none")
        .attr("stroke", "steelblue")
        .attr("stroke-width", 1.5)
        .attr("stroke-linejoin", "round")
        .attr("stroke-linecap", "round")
        .selectAll("path")
        .data(d3.values(cumulative_series))
        .join("path")
        .style("mix-blend-mode", "multiply")
        .attr("d", d => line(d.values))
        .style("stroke", (d, i) => line_color(i));

        lineSVG
        .append("text")
        .datum(d3.values(cumulative_series))
          .attr("transform", d => "translate(" + x(d[0].values.length) + "," + y(d3.max(d[0].values)) + ")")
          .attr("x", 3)
          .attr("dy", "0.35em")
          .style("font", "10px sans-serif")
          .text(d => d[0].name);

        lineSVG
        .append("text")
        .datum(d3.values(cumulative_series))
          .attr("transform", d => "translate(" + x(d[1].values.length) + "," + y(d3.max(d[1].values)) + ")")
          .attr("x", 3)
          .attr("dy", "0.35em")
          .style("font", "10px sans-serif")
          .text(d => d[1].name);

    // Create node data.
    var nodes = d3.keys(stakeholders).map(function(d) {
        // Initialize count for each group.
        groups[stakeholders[d][0].stage].cnt += 1;

        return {
            id: "node"+d,
            x: randomIntFromInterval(1,650),
            y: randomIntFromInterval(20,150),
            r: radius,
            color: groups[stakeholders[d][0].stage].color,
            group: stakeholders[d][0].stage,
            timeleft: stakeholders[d][0].quarters,
            istage: 0,
            stages: stakeholders[d],
            changed: 0,
            investor: stakeholders[d][0].investor_name
        }
    });

    // Circle for each node.
    const circle = svg.append("g")
        .selectAll("circle")
        .data(nodes)
        .join("circle")
          .attr("id", d => d.id)
          .attr("cx", d => d.x)
          .attr("cy", d => d.y)
          .attr("fill", d => d.color)
          .attr("r", d => d.r);

    const counter_rects = {
      "top": {x: 268, y: 62, width: 45, height: 25, color: "red"},
      "mid": {x: 268, y: 242, width: 45, height: 25, color: "yellow"},
      "low": {x: 268, y: 393, width: 45, height: 25, color: "green"}
    }

      var counter_background = svg.selectAll(".counter_back")
         .data(d3.values(counter_rects))
         .enter()
         .append("rect")
         .attr("class", "counter_back")
         .attr("x", d => d.x)
         .attr("y", d => d.y)
         .attr("width", d => d.width)
         .attr("height", d => d.height)
         .attr("fill", "white")
         .attr("stroke-width", "2px")
         .attr("stroke", d => d.color)

     svg.selectAll('.grpcnt')
           .data(d3.keys(groups))
           .join("text")
           .attr("class", "grpcnt")
           .attr("text-anchor", "middle")
           .attr("x", d =>  groups[d].x)
           .attr("y", d => groups[d].y)
           .text(d => groups[d].cnt)
           .attr("font-family", "Graphik Web,Helvetica,sans-serif")
           .attr("font-size", "12px")
           .attr("font-weight", "bold")
           .attr("color", "#434f5c")

      // Make time pass. Adjust node stage as necessary.
      function timer() {
          // Ticker...
          nodes.forEach(function(o,i) {

              o.timeleft -= 1;
              o.changed = 0
              if (o.timeleft == 0 && o.istage < o.stages.length-1) {
                  // Decrease counter for previous group.
                  groups[o.group].cnt -= 1;

                  // Update current node to new group.
                  o.istage += 1;
                  o.group = o.stages[o.istage].stage;
                  o.timeleft = o.stages[o.istage].quarters;

                  // Increment counter for new group.
                  groups[o.group].cnt += 1;
                  // Note the change in states
                  o.changed = 1
              }
          });

          var second = circle.transition("one")
          .filter(d => d.changed === 1 && d.group === "aware")
            .attr("fill", "black")
            .attr("cx", d => randomIntFromInterval(20,650))
            .attr("cy", d => randomIntFromInterval(210,310))

            var third = circle.transition("two")
            .filter(d => d.changed == 1 && d.group == "partner")
            .attr("fill", "red")
            .attr("cx", d => randomIntFromInterval(1,650))
            .attr("cy", d => randomIntFromInterval(380,460))

            // SETUP VORONOI
            const cells = d3.voronoi()
               .x(d => d.attributes["cx"].value)
               .y(d => d.attributes["cy"].value)
              .extent([[-50, 0], [extent_width, extent_height]])
              .polygons(d3.values(circle)[0][0])

            var cells_filtered = cells.filter(function(d) { return d; });

            const cell = svg.append("g")
              .attr("class", "voronoiWrapper")
              .attr("opacity", "0")
              .selectAll("path")
              .data(cells_filtered)
              .enter()

              cell.append("path")
                .attr("stroke", "#orange")
                .attr("d", d => `M${d.join("L")}Z`)
                .on("mouseover", function(d, i) {

                  var xpos = d.data.attributes["cx"].value
                  var ypos = d.data.attributes["cy"].value

                  var label = svg.append("g")
                    .attr("id", "tooltip")
                    .style("pointer-events", "none") // prevents jittering when mouseover both label and voronoi
                    .attr("transform", (d, i) => `translate(${xpos},${ypos})`);
                  label.append("rect")
                    .attr("width", "200")
                    .attr("height", "22")
                    .attr("fill", "orange")
                  label.append("text")
                    .attr("x", 5)
                    .attr("y", 14)
                    .attr("text-anchor", "left")
                    .attr("font-family", "sans-serif")
                    .attr("font-size", "12px")
                    .attr("font-weight", "bold")
                    .attr("fill", "black")
                    .text(d.data.__data__["investor"]);

                })
                .on("mouseout", function(d) {
                  d3.select("#tooltip").remove();
                  d3.select("#guides").remove();
                });


          // Increment time.
          time_so_far += 1;

          if(time_so_far > 17) { return }

          var current_quarter = Math.floor(time_so_far / 13)
          var current_year = Math.floor(time_so_far / 52)

          // stop on the last quarter
          if(time_so_far == d3.keys(customers).length) { return }

          var label_years = Math.floor(time_so_far / 4) == 0 ? "0 Years, " :
                            Math.floor(time_so_far / 4) == 1 ? "1 Year, " :
                            Math.floor(time_so_far / 4) + " Years, "


          var label_quarters = time_so_far % 4 == 0 ? "0 Quarters" :
                                time_so_far % 4 == 1 ? "1 Quarter" :
                                time_so_far % 4 + " Quarters"

          console.log(label_years + label_quarters)
          d3.select("#timecount .cnt").text(label_years + label_quarters);

          // Update counters.
           svg.selectAll('.grpcnt').text(d => groups[d].cnt);

           // add customer counts
           for (i = 0; i < customers[time_so_far].length; i++) {
             console.log("1")
             console.log(customers[time_so_far][i])
             // add by source
             sources[customers[time_so_far][i].source].all_time += customers[time_so_far][i].count
             sources[customers[time_so_far][i].source].Customers += customers[time_so_far][i].count
             // by type
             journey[customers[time_so_far][i].stage].cnt += customers[time_so_far][i].count
           }

           // update counter
           d3.selectAll(".counter")
             .text(d => d.cnt)

           // update table
           table_body.selectAll("tr")
                     .data(d3.values(sources))

           table_body.selectAll("td")
           .attr("class", d => console.log(d))
                     .data(d => columns.map(i => ({column: i, value: d[i]})))
                     .text(d => d.value)

            table_body.sort(function(a, b) {
              return (b["all_time"]) - (a["all_time"]);
            })

           // Define length of a tick
           d3.timeout(timer, tick_time);

      } // @end timer()

      timer()


}); // end TSV
