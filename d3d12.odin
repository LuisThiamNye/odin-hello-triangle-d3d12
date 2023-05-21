package d3d12_example

import "core:fmt"
import "core:mem"
import "core:runtime"

import win "core:sys/windows"
import D3D12 "vendor:directx/d3d12"
import DXGI "vendor:directx/dxgi"
import D3DC "vendor:directx/d3d_compiler"

ISwapChain4 :: struct #raw_union {
	#subtype idxgiswapchain3: DXGI.ISwapChain3,
	using idxgiswapchain4_vtable: ^ISwapChain4_VTable,
}
ISwapChain4_VTable :: struct {
	using idxgiswapchain3_vtable: DXGI.ISwapChain3_VTable,
	SetHDRMetaData: proc "stdcall" (this: ^ISwapChain4, Type: struct{} /*DXGI.HDR_METADATA_TYPE*/, Size: win.UINT, MetaData: rawptr) -> win.HRESULT,
}

SUCCEEDED :: win.SUCCEEDED
HRESULT_FROM_WIN32 :: #force_inline proc(x: u32) -> win.HRESULT {
	FACILITY_WIN32 :: 7
	hr := win.HRESULT(x)
	return hr <= 0 ? hr : win.HRESULT((x & 0x0000FFFF) | (FACILITY_WIN32<<16) | 0x80000000)
}


Window_Size :: struct {
	width: u32,
	height: u32,
}

NUM_RENDERTARGETS :: 2

Renderer_D3D12 :: struct {
	debug: ^D3D12.IDebug,
	factory: ^DXGI.IFactory4,
	adapter: ^DXGI.IAdapter1,
	device: ^D3D12.IDevice,
	queue: ^D3D12.ICommandQueue,
	swapchain: ^ISwapChain4,
	rtv_descriptor_heap: ^D3D12.IDescriptorHeap,
	command_allocator: ^D3D12.ICommandAllocator,
	root_signature: ^D3D12.IRootSignature,
	pipeline: ^D3D12.IPipelineState,
	cmdlist: ^D3D12.IGraphicsCommandList,
}

Resources_D3D12 :: struct {
	targets: [NUM_RENDERTARGETS]^D3D12.IResource,
	vertex_buffer: ^D3D12.IResource,
	vertex_buffer_view: D3D12.VERTEX_BUFFER_VIEW,
	fence: ^D3D12.IFence,
	fence_event: win.HANDLE,
	fence_value: u32,
	frame_index: u32,
}

get_descriptor_handle_d3d12 :: proc(
	heap: ^D3D12.IDescriptorHeap,
	heapType: D3D12.DESCRIPTOR_HEAP_TYPE,
	#any_int index: int,
	device: ^D3D12.IDevice) -> (handle: D3D12.CPU_DESCRIPTOR_HANDLE) {

	heap->GetCPUDescriptorHandleForHeapStart(&handle)

	if index > 0 {
		size := device->GetDescriptorHandleIncrementSize(heapType)
		handle.ptr += uint(index * int(size))
	}

	return handle
}

deinit_d3d12 :: proc(renderer: ^Renderer_D3D12, resources: ^Resources_D3D12) {
	using D3D12
	wait_for_frame(renderer, resources)

	// free resources
	resources.vertex_buffer->Release()
	resources.fence->Release()
	win.CloseHandle(resources.fence_event)

	for i in 0..< NUM_RENDERTARGETS {
		resources.targets[i]->Release()
	}

	// free renderer
	if renderer.debug != nil {
		renderer.debug->Release()
	}
	renderer.factory->Release()
	renderer.adapter->Release()
	renderer.device->Release()
	renderer.queue->Release()
	renderer.swapchain->Release()
	renderer.rtv_descriptor_heap->Release()
	renderer.command_allocator->Release()
	renderer.root_signature->Release()
	renderer.pipeline->Release()
	renderer.cmdlist->Release()
}

wait_for_frame :: proc(renderer: ^Renderer_D3D12, resources: ^Resources_D3D12) {
	using D3D12
	value := u64(resources.fence_value)

	if hr := renderer.queue->Signal(resources.fence, value); !SUCCEEDED(hr) {
		fmt.printf("Failed to signal fence: %d\n", hr)
		return
	}

	resources.fence_value += 1	
	
	completed := resources.fence->GetCompletedValue()
	if completed < value {
		hr := resources.fence->SetEventOnCompletion(value, resources.fence_event)
		if !SUCCEEDED(hr) {
			fmt.printf("Failed to set event on completion flag: %d\n", hr)
		}
		win.WaitForSingleObject(resources.fence_event, win.INFINITE)
	}

	resources.frame_index = renderer.swapchain->GetCurrentBackBufferIndex()
}

init_hello_triangle :: proc(hWindow: win.HWND, renderer: ^Renderer_D3D12, resources: ^Resources_D3D12, ws: Window_Size) -> bool {
	using D3D12

	////////////////////////////////////////////////////////////////////////////////	
	// debug reporting
	when ODIN_DEBUG {
		if SUCCEEDED(GetDebugInterface(IDebug_UUID, cast(^rawptr)&renderer.debug)) {
			renderer.debug->EnableDebugLayer()
		} else {
			fmt.printf("Failed to get debug interface\n")
			return false
		}
	}

	////////////////////////////////////////////////////////////////////////////////	
	// create pipeline objects
	{
		flags : u32 = 0
		when ODIN_DEBUG {
			flags |= DXGI.CREATE_FACTORY_DEBUG
		}

		hr := DXGI.CreateDXGIFactory2(flags, DXGI.IFactory4_UUID, cast(^rawptr)&renderer.factory)
		if !SUCCEEDED(hr) {
			fmt.printf("Failed to create factory: %u\n", hr)
			return false
		}
	}

	for i := 0; DXGI.ERROR_NOT_FOUND != renderer.factory->EnumAdapters1(u32(i), &renderer.adapter); i+=1 {
		desc : DXGI.ADAPTER_DESC1
		renderer.adapter->GetDesc1(&desc)
		if desc.Flags & u32(DXGI.ADAPTER_FLAG.SOFTWARE) > 0 {
			continue
		}
		hr := CreateDevice(cast(^DXGI.IUnknown)renderer.adapter, FEATURE_LEVEL._11_0, IDevice_UUID, nil)
		if SUCCEEDED(hr) {
			break
		} else {
			fmt.printf("Failed to create device: %u\n", hr)
		}
	}

	if (renderer.adapter == nil) {
		fmt.printf("No hardware adapter found. This system does not support .\n")
		return false
	}

	{
		hr := CreateDevice(
			cast(^DXGI.IUnknown)renderer.adapter, 
			FEATURE_LEVEL._11_0, 
			IDevice_UUID, 
			cast(^rawptr)&renderer.device)
		if !SUCCEEDED(hr) {
			fmt.printf("Failed to create device: %u\n", hr)
			return false
		}
	}

	{
		desc := COMMAND_QUEUE_DESC{Type = .DIRECT}

		hr := renderer.device->CreateCommandQueue(&desc, ICommandQueue_UUID, cast(^rawptr)&renderer.queue)
		if !SUCCEEDED(hr) {
			fmt.printf("Failed to create command queue: %u", hr)
			return false
		}
	}

	{
		desc := DXGI.SWAP_CHAIN_DESC1{
			Width = ws.width,
			Height = ws.height,
			Format = .R8G8B8A8_UNORM,
			SampleDesc = {
				Count = 1,
				Quality = 0,
			},
			BufferUsage = {.RENDER_TARGET_OUTPUT},
			BufferCount = NUM_RENDERTARGETS,
			Scaling = DXGI.SCALING.NONE,
			SwapEffect = .FLIP_DISCARD,
			AlphaMode = .UNSPECIFIED,
		}

		hr := renderer.factory->CreateSwapChainForHwnd(
			renderer.queue, hWindow, &desc, nil, nil, cast(^^DXGI.ISwapChain1)&renderer.swapchain)
		if !SUCCEEDED(hr) {
			fmt.printf("Failed to create swap chain: %u\n", hr)
			return false
		}
	}
	
	// disable fullscreen transitions
	renderer.factory->MakeWindowAssociation(hWindow, u32(DXGI.MWA.NO_ALT_ENTER))

	resources.frame_index = renderer.swapchain->GetCurrentBackBufferIndex()

	{
		desc := DESCRIPTOR_HEAP_DESC{
			NumDescriptors = NUM_RENDERTARGETS,
			Type = .RTV,
			Flags = {},
		}

		hr := renderer.device->CreateDescriptorHeap(
			&desc, IDescriptorHeap_UUID, cast(^rawptr)&renderer.rtv_descriptor_heap)
		if !SUCCEEDED(hr) {
			fmt.printf("Failed to create descriptor heap: %u\n", hr)
			return false
		}
	}

	{
		rtvDescriptorSize :=
			renderer.device->GetDescriptorHandleIncrementSize(.RTV)

		rtvDescriptorHandle :=
			get_descriptor_handle_d3d12(renderer.rtv_descriptor_heap, .RTV, 0, renderer.device)

		for i in 0..<NUM_RENDERTARGETS {
			hr := renderer.swapchain->GetBuffer(u32(i), IResource_UUID, cast(^rawptr)&resources.targets[i])
			if !SUCCEEDED(hr) {
				return false
			}
			renderer.device->CreateRenderTargetView(resources.targets[i], nil, rtvDescriptorHandle)
			rtvDescriptorHandle.ptr += uint(rtvDescriptorSize)
		}
	}

	{
		hr := renderer.device->CreateCommandAllocator(
			.DIRECT,
			ICommandAllocator_UUID,
			cast(^rawptr)&renderer.command_allocator)
		if !SUCCEEDED(hr) {
			fmt.printf("Failed to create command allocator: %u\n", hr)
			return false
		}
	}

	////////////////////////////////////////////////////////////////////////////////
	// create pipline assets
	{
		desc := VERSIONED_ROOT_SIGNATURE_DESC{
			Version = ._1_0,
			Desc_1_0 = {
				Flags = {.ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT},
			},
		}
		serializedDesc : ^IBlob
		hr := SerializeVersionedRootSignature(&desc, &serializedDesc, nil)
		if !SUCCEEDED(hr) {
			fmt.printf("Failed to serialize root signature: %u\n", hr)
			return false
		}

		hr = renderer.device->CreateRootSignature(
			0,
			serializedDesc->GetBufferPointer(),
			serializedDesc->GetBufferSize(),
			IRootSignature_UUID,
			cast(^rawptr)&renderer.root_signature)

		if !SUCCEEDED(hr) {
			fmt.printf("Failed to create root signature: %u\n", hr)
			return false
		}

		serializedDesc->Release()
	}

	{
		data := `
struct PSInput {
	float4 position : SV_POSITION;
	float4 color : COLOR;
};
PSInput VSMain(float4 position : POSITION0, float4 color : COLOR0) {
	PSInput result;
	result.position = position;
	result.color = color;
	return result;
}
float4 PSMain(PSInput input) : SV_TARGET {
	return input.color;
}
`

		data_size := uint(len(data))

		compileFlags : D3DC.D3DCOMPILE
		when ODIN_DEBUG {
			compileFlags += {.DEBUG, .SKIP_OPTIMIZATION}
		}

		vs : ^IBlob
		ps : ^IBlob

		hr := D3DC.Compile(raw_data(data), data_size, nil, nil, nil, "VSMain", "vs_4_0", transmute(u32)(compileFlags), 0, &vs, nil)
		if !SUCCEEDED(hr) {
			fmt.printf("Failed to compile vertex shader: %u\n", hr)
			return false
		}
		hr = D3DC.Compile(raw_data(data), data_size, nil, nil, nil, "PSMain", "ps_4_0", transmute(u32)(compileFlags), 0, &ps, nil)
		if !SUCCEEDED(hr) {
			fmt.printf("Failed to compile vertex shader: %u\n", hr)
			return false
		}

		vertexFormat := [?]INPUT_ELEMENT_DESC{
			{ 
				SemanticName = "POSITION", 
				Format = .R32G32B32_FLOAT, 
				InputSlotClass = .PER_VERTEX_DATA, 
			},
			{ 	
				SemanticName = "COLOR", 
				Format = .R32G32B32A32_FLOAT, 
				AlignedByteOffset = size_of(f32) * 3, 
				InputSlotClass = .PER_VERTEX_DATA, 
			},
		}

		defaultBlendState := RENDER_TARGET_BLEND_DESC{
			BlendEnable = false,
			LogicOpEnable = false,

			SrcBlend = .ONE,
			DestBlend = .ZERO,
			BlendOp = .ADD,

			SrcBlendAlpha = .ONE,
			DestBlendAlpha = .ZERO,
			BlendOpAlpha = .ADD,

			LogicOp = .NOOP,
			RenderTargetWriteMask = u8(COLOR_WRITE_ENABLE_ALL),
		}

		pipelineStateDesc := GRAPHICS_PIPELINE_STATE_DESC{
			pRootSignature = renderer.root_signature,
			VS = {
				pShaderBytecode = vs->GetBufferPointer(),
				BytecodeLength = vs->GetBufferSize(),
			},
			PS = {
				pShaderBytecode = ps->GetBufferPointer(),
				BytecodeLength = ps->GetBufferSize(),
			},
			StreamOutput = {},
			BlendState = {
				AlphaToCoverageEnable = false,
				IndependentBlendEnable = false,
				RenderTarget = { defaultBlendState, {}, {}, {}, {}, {}, {}, {}},
			},
			SampleMask = 0xFFFFFFFF,
			RasterizerState = {
				FillMode = .SOLID,
				CullMode = .BACK,
				FrontCounterClockwise = false,
				DepthBias = 0,
				DepthBiasClamp = 0,
				SlopeScaledDepthBias = 0,
				DepthClipEnable = true,
				MultisampleEnable = false,
				AntialiasedLineEnable = false,
				ForcedSampleCount = 0,
				ConservativeRaster = .OFF,
			},
			DepthStencilState = {
				DepthEnable = false,
				StencilEnable = false,
			},
			InputLayout = {
				pInputElementDescs = &vertexFormat[0],
				NumElements = len(vertexFormat),
			},
			PrimitiveTopologyType = .TRIANGLE,
			NumRenderTargets = 1,
			RTVFormats = {.R8G8B8A8_UNORM, {}, {}, {}, {}, {}, {}, {}},
			DSVFormat = .UNKNOWN,
			SampleDesc = {
				Count = 1,
				Quality = 0,
			},
		}
		
		hr = renderer.device->CreateGraphicsPipelineState(
			&pipelineStateDesc, IPipelineState_UUID, cast(^rawptr)&renderer.pipeline)
		if !SUCCEEDED(hr) {
			fmt.printf("Failed to create pipeline state: %u\n", hr)
			return false
		}

		vs->Release()
		ps->Release()
	}

	{
		hr := renderer.device->CreateCommandList(
			0, 
			COMMAND_LIST_TYPE.DIRECT, 
			renderer.command_allocator, 
			renderer.pipeline,
			ICommandList_UUID,
			cast(^rawptr)&renderer.cmdlist)
		if !SUCCEEDED(hr) {
			fmt.printf("Failed to create command list: %u\n", hr)
			return false
		}

		// command lists begin in the recording state, but the update loop opens it
		hr = renderer.cmdlist->Close()
		if !SUCCEEDED(hr) {
			fmt.printf("Failed to close command list: %u\n", hr)
			return false
		}
	}

	{
		aspect := f32(ws.width) / f32(ws.height)

		vertices := [?]f32{
			// pos                 colour
			 0.00,  0.25 * aspect, 0.0,	1,0,0,0,
			 0.25, -0.25 * aspect, 0.0, 0,1,0,0,
			-0.25, -0.25 * aspect, 0.0, 0,0,1,0,
		}

		heapProps := HEAP_PROPERTIES{
			Type = .UPLOAD,
		}

		resourceDesc := RESOURCE_DESC{
			Dimension = .BUFFER,
			Alignment = 0,
			Width = size_of(vertices),
			Height = 1,
			DepthOrArraySize = 1,
			MipLevels = 1,
			Format = .UNKNOWN,
			SampleDesc = {Count = 1, Quality = 0},
			Layout = .ROW_MAJOR,
			Flags = {},
		}

		hr := renderer.device->CreateCommittedResource(
			&heapProps, 
			{},
			&resourceDesc,
			RESOURCE_STATE_GENERIC_READ, 
			nil,
			IResource_UUID,
			cast(^rawptr)&resources.vertex_buffer)
		if !SUCCEEDED(hr) {
			fmt.printf("Failed to create vertex buffer resource: %d\n", hr)
			return false
		}

		gpuData : rawptr
		readRange : RANGE // cpu isn't going to read this data, only write
		hr = resources.vertex_buffer->Map(0, &readRange, &gpuData)
		if !SUCCEEDED(hr) {
			fmt.printf("Failed to create vertex buffer resource: %d\n", hr)
			return false
		}
		mem.copy(gpuData, &vertices[0], size_of(vertices))
		resources.vertex_buffer->Unmap(0, nil)

		vbView := VERTEX_BUFFER_VIEW{
			BufferLocation = resources.vertex_buffer->GetGPUVirtualAddress(),
			StrideInBytes = size_of(f32) * (3 + 4),
			SizeInBytes = size_of(vertices),
		}
		resources.vertex_buffer_view = vbView
	}

	{
		hr := renderer.device->CreateFence(
			u64(resources.fence_value), {}, IFence_UUID, cast(^rawptr)&resources.fence)
		if !SUCCEEDED(hr) {
			fmt.printf("Failed to create fence: %d\n", hr)
			return false
		}
		resources.fence_value += 1

		manualReset := false
		initialState := false
		resources.fence_event = win.CreateEventW(nil, win.BOOL(manualReset), win.BOOL(initialState), nil)
		if (resources.fence_event == nil) {
			hr = HRESULT_FROM_WIN32(win.GetLastError())
			fmt.printf("Failed to create fence event: %d\n", hr)
			return false
		}
	}

	// wait for gpu to finish work before continuing
	wait_for_frame(renderer, resources)
	return true
}

draw_hello_triangle :: proc(renderer: ^Renderer_D3D12, resources: ^Resources_D3D12, ws: Window_Size) {
	using D3D12
	// populate command list

	// use fences to make sure associated command lists are finished executing
	hr := renderer.command_allocator->Reset()
	if !SUCCEEDED(hr) {
		fmt.printf("Failed to reset command allocator: %d\n", hr)
		return
	}

	hr = renderer.cmdlist->Reset(renderer.command_allocator, renderer.pipeline)
	if !SUCCEEDED(hr) {
		fmt.printf("Failed to reset command list: %d\n", hr)
		return
	}

	viewport := VIEWPORT{
		TopLeftX = 0.0,
		TopLeftY = 0.0,
		Width = f32(ws.width),
		Height = f32(ws.height),
		MinDepth = 0.0,
		MaxDepth = 0.0,
	}

	scissorRect := RECT{
		left = 0, right = i32(ws.width),
		top = 0, bottom = i32(ws.height),
	}

	// This state is reset everytime the cmd list is reset, so we need to rebind it
	renderer.cmdlist->SetGraphicsRootSignature(renderer.root_signature)
	renderer.cmdlist->RSSetViewports(1, &viewport)
	renderer.cmdlist->RSSetScissorRects(1, &scissorRect)

	toRenderTargetBarrier := RESOURCE_BARRIER{
		Type = .TRANSITION,
		Flags = {},
		Transition = {
			pResource = resources.targets[resources.frame_index],
			StateBefore = RESOURCE_STATE_PRESENT,
			StateAfter = {.RENDER_TARGET},
			Subresource = RESOURCE_BARRIER_ALL_SUBRESOURCES,
		},
	}
	renderer.cmdlist->ResourceBarrier(1, &toRenderTargetBarrier)

	rtvHandle := get_descriptor_handle_d3d12(
		renderer.rtv_descriptor_heap,
		DESCRIPTOR_HEAP_TYPE.RTV,
		resources.frame_index,
		renderer.device)
	renderer.cmdlist->OMSetRenderTargets(1, &rtvHandle, false, nil)

	// clear backbuffer
	clearcolor := [?]f32{ 0.05, 0.05, 0.05, 1.0}
	renderer.cmdlist->ClearRenderTargetView(rtvHandle, &clearcolor, 0, nil)

	// draw calls!
	renderer.cmdlist->IASetPrimitiveTopology(.TRIANGLELIST)
	renderer.cmdlist->IASetVertexBuffers(0, 1, &resources.vertex_buffer_view)
	renderer.cmdlist->DrawInstanced(3, 1, 0, 0)
	
	toPresentBarrier := toRenderTargetBarrier
	toPresentBarrier.Transition.StateBefore = {.RENDER_TARGET}
	toPresentBarrier.Transition.StateAfter = RESOURCE_STATE_PRESENT

	renderer.cmdlist->ResourceBarrier(1, &toPresentBarrier)
	hr = renderer.cmdlist->Close()
	if !SUCCEEDED(hr) {
		fmt.printf("Failed to close command list: %d\n", hr)
	}

	// execute
	cmdlists := [?]^IGraphicsCommandList{renderer.cmdlist}
	renderer.queue->ExecuteCommandLists(len(cmdlists), cast(^^ICommandList)&cmdlists[0])

	// present
	{
		flags : u32 = 0
		params : DXGI.PRESENT_PARAMETERS
		hr := renderer.swapchain->Present1(1, flags, &params)
		if !SUCCEEDED(hr) {
			fmt.printf("Failed to present backbuffer: %u\n", hr)
		}
	}
}

App_State :: struct {
	renderer: ^Renderer_D3D12,
	resources: ^Resources_D3D12,
	window_size: Window_Size,
}

window_proc :: proc "stdcall" (hWindow: win.HWND, msg: win.UINT,
	wparam: win.WPARAM, lparam: win.LPARAM) -> win.LRESULT {
	using win
	context = runtime.default_context()

	switch msg {
	case WM_CREATE:
		info := (^CREATESTRUCTW)(uintptr(lparam))
		userdata := info.lpCreateParams
		SetLastError(0)
		if (SetWindowLongPtrW(hWindow, GWLP_USERDATA, int(uintptr(userdata))) == 0 &&
			GetLastError() != 0) {
			fmt.printf("Something went wrong setting window's userdata: %d\n", GetLastError())
		}
		return 0

	case WM_DESTROY:
	case WM_CLOSE:
		PostQuitMessage(0)
		return 0

	case WM_PAINT:
		params := cast(^App_State)uintptr(GetWindowLongPtrW(hWindow, GWLP_USERDATA))
		draw_hello_triangle(params.renderer, params.resources, params.window_size)
		wait_for_frame(params.renderer, params.resources)
		return 0
	}
	return DefWindowProcW(hWindow, msg, wparam, lparam)
}

main :: proc() {
	using win

	////////////////////////////////////////////////////////////////////////////////	
	// Create window

	hInstance := GetModuleHandleW(nil)

	win_class: WNDCLASSEXW
	win_class.cbSize = size_of(WNDCLASSEXW)
	win_class.style = CS_HREDRAW | CS_VREDRAW
	win_class.lpfnWndProc = window_proc
	win_class.hInstance = HANDLE(hInstance)
	win_class.lpszClassName = utf8_to_wstring("HelloTriangleWindow")
	if RegisterClassExW(&win_class) == 0 {
		fmt.printf("Failed to create window class.")
		return
	}

	WINDOW_WIDTH:  u32 = 1280
	WINDOW_HEIGHT: u32 = 720

	app: App_State

	hWindow := CreateWindowExW(
		0,
		utf8_to_wstring("HelloTriangleWindow"),
		utf8_to_wstring("Hello Triangle"),
		WS_OVERLAPPEDWINDOW | WS_VISIBLE | WS_SYSMENU,
		100, 100,
		i32(WINDOW_WIDTH), i32(WINDOW_HEIGHT),
		nil,
		nil,
		HANDLE(hInstance),
	 &app)
	if hWindow == nil {
		fmt.printf("Failed to create window.")
		return
	}

	ws := Window_Size{width=WINDOW_WIDTH, height=WINDOW_HEIGHT}
	renderer: Renderer_D3D12
	resources: Resources_D3D12

	init_hello_triangle(hWindow, &renderer, &resources, ws)

	app = App_State{
		renderer = &renderer,
		resources = &resources,
		window_size = ws,
	}

	////////////////////////////////////////////////////////////////////////////////
	// main loop

	for {
		msg: MSG
		PeekMessageW(&msg, nil, 0, 0, PM_REMOVE)
		if msg.message == WM_QUIT {
			break
		} else {
			TranslateMessage(&msg)
			DispatchMessageW(&msg)
		}
	}

	deinit_d3d12(&renderer, &resources)

	return
}