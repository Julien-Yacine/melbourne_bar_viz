# Melbourne bar vizualisation with leaflet and R

![Alt text](https://github.com/Julien-Yacine/melbourne_bar_viz/blob/master/melbourne_bar_voronoi_map.png?raw=true "Title")

* **scrape_data.py**: retrieve informations about the melbournian bar from the OpenStreetMap api - *based on the work of [Julien Moura](https://github.com/Guts/Paris-Beer-Week/blob/master/data/raw_data/getOpenBeerMap.py)* 
* **melbourne_bar_viz.geojson**: data generated by scrape_data.py
* **melbourne_bar_viz.R**: Vizualisation of the data using leaflet - *Voronoï diagram*, *heatmap*
