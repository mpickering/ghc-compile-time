H = nx.OrderedDiGraph(nx.transitive_reduction(G))
H.add_nodes_from(G.nodes.items())
H.add_edges_from((u, v, G[u][v]) for (u, v) in H.edges())
H.graph.update(G.graph)
G=H
for node, outdegree in G.out_degree(G.nodes()):
    if outdegree == 0:
        print(node)
        G.add_edge(node, "SINK", weight=0.01)

for node, indegree in G.in_degree(G.nodes()):
    if indegree == 0:
        print(node)
        G.add_edge("SOURCE", node, weight=0.01)

for node, outdegree in G.out_degree(G.nodes()):
    if outdegree == 0:
        print(node)

for node, indegree in G.in_degree(G.nodes()):
    if indegree == 0:
        print(node)

def single_source_longest_dag_path_length(graph, s):
        assert(graph.in_degree(s) == 0)
        dist = dict.fromkeys(graph.nodes, -float('inf'))
        dist[s] = 0
        topo_order = nx.topological_sort(graph)
        for n in topo_order:
            for s in graph.successors(n):
                if dist[s] < dist[n] + graph.edges[n,s]['weight']:
                    dist[s] = dist[n] + graph.edges[n,s]['weight']
        return dist

def single_source_longest_dag_path_length_unweight(graph, s):
        assert(graph.in_degree(s) == 0)
        dist = dict.fromkeys(graph.nodes, -float('inf'))
        dist[s] = 0
        topo_order = nx.topological_sort(graph)
        for n in topo_order:
            for s in graph.successors(n):
                if 'capacity' in graph.edges[n,s]:
                    cap = graph.edges[n,s]['capacity']
                else:
                    cap = 0
                cap = 1
                if  dist[s] < dist[n] + cap:
                    dist[s] = dist[n] + cap
        return dist


def remove_prefix(text, prefix):
    if text.startswith(prefix):
        return text[len(prefix):]
    return text

longest = nx.dag_longest_path(G)
longest_length = nx.dag_longest_path_length(G)
print(longest)
total_time = sum([d['weight'] for (u, v, d) in G.edges(data=True)])
print("Critical path length: {0:.2f}s".format(longest_length / 1000))
print("Actual time: {0:.2f}s".format(333889.5964355469 / 1000))
caps = 8
print("Theoretical minimum build time ({0:d} cores): {1:.2f}s".format(caps, (total_time / (1000 * caps))))

mc = nx.minimum_cut(G, "SOURCE", "SINK")
print(mc)

longest_length2 = nx.dag_longest_path_length(G, weight="capacity")
print("If each module took 1s to compile..: {0:.2f}s".format(longest_length2))

fig, ax = plt.subplots(figsize=(100,100))

G=nx.relabel_nodes(G, lambda x: remove_prefix(x, "END:"))
G.remove_edges_from(nx.selfloop_edges(G))
ls = single_source_longest_dag_path_length(G, "SOURCE")
print(ls)
ls = single_source_longest_dag_path_length_unweight(G, "SOURCE")
nx.set_node_attributes(G, ls, "subset")
pos = nx.multipartite_layout(G, align="horizontal", scale=1)
event_state = dict([ (node, "IDLE") for node in G.nodes()])
n_jobs = 0

Writer = animation.writers['ffmpeg']
writer = Writer(fps=1, metadata=dict(artist='Me'), bitrate=30000)

def update(frames):
    global events, n_jobs, event_state
    current_time = frames * 1000
    print(current_time)
    take, drop = itertools.tee(events)
    new_events = itertools.takewhile(lambda x: x[0] < current_time, take)
    events = itertools.dropwhile(lambda x: x[0] < current_time, drop)
    for (t, node, event) in new_events:
        print(t, node, event)
        if event == "Start":
            event_state[node] = "RUNNING"
            n_jobs += 1
        if event == "End":
            event_state[node] = "FINISHED"
            n_jobs -= 1

    waiting = [n for n, d in event_state.items() if d == "IDLE"]
    running = [n for n, d in event_state.items() if d == "RUNNING"]
    finished = [n for n, d in event_state.items() if d == "FINISHED"]

    plt.cla()
    options = {"node_size": 30}
    nx.draw_networkx_nodes(G, pos, nodelist=waiting, node_color="red", alpha=0.3, **options)
    nx.draw_networkx_nodes(G, pos, nodelist=running, node_color="green", alpha=0.5, **options)
    nx.draw_networkx_nodes(G, pos, nodelist=finished, node_color="black", alpha=0.5, **options)
    nx.draw_networkx_edges(G, pos, width=1.0, alpha=0.2)
    #nx.draw_networkx_labels(G, pos, font_color="red", horizontalalignment='left', font_size=8, verticalalignment="bottom")
    plt.text(pos['SOURCE'][0],pos['SOURCE'][1] - 0.1, str(n_jobs))
    plt.text(pos['SOURCE'][0],pos['SOURCE'][1] - 0.15, "%ds" % frames)

#nx.draw(G, pos, with_labels=False)
#nx.draw_networkx_labels(G, pos, font_color="red", horizontalalignment='left', font_size=8, verticalalignment="bottom")
ani = animation.FuncAnimation(fig, update, frames=(math.ceil(events[-1][0]/1000)+10), interval=200)
#ani.save('modules.mp4', writer=writer)
update(0)
plt.show()
