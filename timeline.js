
var fformat = {
  minorLabels: {
    millisecond:'SSS',
    second:     's',
    minute:     'mm',
    hour:       '',
    weekday:    '',
    day:        '',
    week:       '',
    month:      '',
    year:       ''
  },
  majorLabels: {
    millisecond:'',
    second:     'mm',
    minute:     'HH:mm',
    hour:       '',
    weekday:    '',
    day:        '',
    week:       '',
    month:      '',
    year:       ''
  }
};

var options = {
    stack: true,
    maxHeight: 640,
    format: fformat,
    zoomKey: "ctrlKey",
    showCurrentTime: false,
    orientation: {
        axis: "bottom",
        item: "top"
    },
};
var items = new vis.DataSet();

module_data.forEach(function (item, index){
    items.add({
        id: index,
        group: 0,
        start: item.start,
        end: item.end,
        type: 'range',
        title: item.name,
        content: item.name
    });
});


groups = new vis.DataSet();
groups.add({id:0, content: 'Modules'});

// create a Timeline
var container = document.getElementById('visualization');
timeline = new vis.Timeline(container, null, options);
timeline.setGroups(groups);
timeline.setItems(items);

