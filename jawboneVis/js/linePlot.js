$(function() {
    $.getJSON('monthDistance.json', function(response){
       dataset = response;
    }).success(ShowChart);  
});

function setupChart(){
    console.log('json loaded');

    var HighChartObject = function() {

        this.chart = {
            type: 'line',
            zoomType: 'xy'
        }

        this.title = {
            text: ''
        }

        this.xAxis = {}

        this.yAxis = {
            title: {
                text: 'Avg Distance (Km)'
            }
        }

        this.plotOptions = {
            line: {
                dataLabels: {
                    enabled: true
                },
                enableMouseTracking: false,
                color: "#6517E8"
            }
        }

        this.series = {}
    };

    function CreateXAxis(){
        return {
                categories: dataset.names
            }
    };

    function CreateSeries(){
        return [{
            showInLegend : false,
            data : dataset.averages
        }]
    };  

    var chart = new HighChartObject();
    chart.xAxis = CreateXAxis();
    chart.series = CreateSeries();

    return chart;
}

function ShowChart(dataset){
      $("#container").highcharts(setupChart(dataset));
     
}