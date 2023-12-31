# RISS_NETWORK_ANALYSIS

**매개중심성을 파악하고, 상위 10개의 노드를 파악하며 이를 이용하여 '갈등'에 대한 시기별 주요 키워드를 추출한다.**  

**추출 기준**
- 노드의 크기 및 연결강도, 레이아웃 분석
- 연도별 주요 주제의 흐름파악

****→ 객관적 지표와 배경지식을 활용한 주관적 파악을 통해 최종 키워드를 추출****

**중심성 지표 설정**       
→ 논문들 간의 연결 및 관계를 파악하고, 특정 주제나 키워드 주변에 어떤 논문들이 집중되어 있는지 확인하는데 유리한 중심성으로 설정
    
  ****1. 매개 중심성 (Betweenness Centrality):**** 
- 단어와 단어들을 연결해주는 중계자 역할을 하는 노드를 파악할 수 있음
- 매개 중심성은 논문들 간의 정보 흐름을 파악하는 데 도움이 됨. 중요한 주제나 연구 영역을 중심으로 다른 논문들과 얼마나 관련이 있는지를 측정할 수 있음
- 매개 중심성이 높은 논문은 특정 주제에 대한 중요한 중개자 역할을 할 수 있으며, 해당 주제의 연구 동향을 이해하는 데 좋음   

  ![image](https://github.com/silverwater1820/RISS_NETWORK_ANALYSIS/assets/97444162/29fa24d7-a32d-4a41-8529-edfb7ebb0635)   

매개중심성 → 다리역할을 하는 중개자를 찾을 때 유용하게 사용된다. 보통 매개중심성은 노드와 노드사이를 최단경로로 연결하는 위치에 있는 노드를 의미한다. 최단경로에 위치할수록 노드사이의 매개중심성 값은 높아진다. 만일 매개역할을 하는 노드(최단거리경로)가 여러 개 있다면 **각각의 노드가 가지는 매개중심성은 낮아진다.** 왜냐하면, x, y, z 3개의 노드가 있을 때 y의 매개중심성은 x, z를 경유하는 최단거리경로 수의 비율로 계산되기 때문이다.
        
  ****2. 연결 중심성(Degree Centrality):****   
- 연결 중심성은 논문이 다른 논문들과 얼마나 많이 연결되어 있는지를 측정.
- 특정 주제와 관련이 있는 논문들이 다른 논문들과 밀접하게 연결되어 있을 가능성이 높음
- 연결 중심성이 높은 논문은 해당 주제의 핵심 논문일 수 있으며, 해당 주제의 중요성을 나타낼 수 있음   

  ![image](https://github.com/silverwater1820/RISS_NETWORK_ANALYSIS/assets/97444162/87470711-7e6e-44b8-adb7-9f057ba61e88)

**사용한 데이터**     
RISS에서 크롤링한 2013년도 ~2023년도 총 10년간의 논문 제목

        
