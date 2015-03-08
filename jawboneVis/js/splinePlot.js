$(function() {
	$.getJSON('dayDistance.json', function(response){
       splineDataset = response;
 	}).success(ShowSplineChart);	
});

// returns an highchart configuration object
function SetUpHighCharts(splineDataset) {
	console.log('spline json loaded');

    function CreateXAxis(){
        return {
            type: 'datetime',
            tickInterval: 3600 * 1000,
            labels: {
                formatter: function () {
                    return Highcharts.dateFormat('%d-%m-%Y', new Date(this.value*1000));
                }
            }
        };
    };

    function CreateSplineSeries(){
        return [{
            showInLegend : false,
            data : splineDataset.utcString
        }]
    };  

    var SplineHighchartObj = function () {
        this.chart = {
            type: 'spline'
        };//for spline with irregular data

        this.title = {
                text: 'Daily Walked Distance',
                x: -20 //center
        };

        this.xAxis = {};

        this.yAxis = {
            title: {
                text: 'Distance (Km)'
            },
            plotLines: [{
                value: 0,
                width: 1,
                color: '#808080'
            }]
        };

        this.tooltip = {            
            formatter: function(){
                return  "<b>" + Highcharts.dateFormat('%d-%m-%Y', this.x*1000) + "</b><br/>" + this.y + " km"
            }            
        };

        this.legend = {
            layout: 'vertical',
            align: 'right',
            verticalAlign: 'middle',
            borderWidth: 0
        };

        this.series = {};
    };

    var splineChart = new SplineHighchartObj();
    splineChart.xAxis = CreateXAxis();
    splineChart.series = CreateSplineSeries();

    return splineChart;
}

// draws an spline highchart
function ShowSplineChart(splineDataset) {
    $("#container").highcharts(SetUpHighCharts(splineDataset));
}